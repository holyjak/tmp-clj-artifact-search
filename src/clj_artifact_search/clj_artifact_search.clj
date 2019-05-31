;; TODO Take download count into account - see "Integrating field values into the score" at https://lucene.apache.org/core/8_0_0/core/org/apache/lucene/search/package-summary.html#changingScoring
;; TODO Optimize: try various searches => adjust query type, weights
;; TODO Review, simplify
;; TODO Find out how to prioritize shorter matches, e.g. for "re-fram" -> "re-frame" desired 1st
(ns clj-artifact-search.clj-artifact-search
  "Index and search Maven/Clojars artifacts.

  Requirements:
   1. Prioritize artifact id > group id > description.
   2. Boost if match both in artifact id and group id, such as in 're-frame:re-frame'.
   3. Boost the shortest match.
   4. Prefix search - not necessary to type the whole name, e.g. 'concord' -> 'concordia'.
   5. Word search - match even just a single word from the name, e.g. 'http' -> 'clj-http' for artifact-id
     and 'nervous' -> 'io.nervous' from the group-id. But match the whole thing too, e.g.
     'clj-nyam' -> 1. 'clj-nyam' 2. 'nyam'. Especially important for names like 're-frame' where - is an
     integral part of the name.
   6. Boost by download count so that if multiple artifacts with similar score, the most popular ones come first.
   7. (?) Most similar match when no direct one - if I search for 'clj-http2' but there is no such package, only 'http2',
      I want to get 'http2' back despite the fact there is no 'clj' in it.

  Examples:
   conc -> clj-concordion
   ring -> ring:ring-core
   clojure -> org.clojure:clojure, org.clojure:clojurescript
   nyam -> clj-nyam
   re-frame -> re-frame with re-frame:re-frame first (even if disregarding download count)
   frame -> re-frame:re-frame
   RiNg -> ring
   nervous ->io.nervous:* artifacts

  Decisions:
   1. Search each field individually so that we can boost artifact-id > group-id > description.
   2. Don't use RegexpQuery by default, i.e. 'clj-http' -> '.*clj-http.*' because:
      a) I'm not sure whether it is good w.r.t. our scoring preferences - prefer the shortest match,
         prefer to match at word start rather than its middle
      b) Tokenizing the names and description makes it possible to find similar artifacts when no direct match.
      c) Prepending '.*' (so we can match 'http' -> 'clj-http') makes the query slow (and we run it on 3 fields);
         but maybe not relevant with our data set size?
   3. Break names at '.', '-' so that we can search for their meaningful parts, see #5 above.
   4. Use PrefixQuery so that the whole name does not need to be typed.
   5. Add boost to artifact id then group id to support req. 1.
   6. Include '-raw' fields for group, artifact so that we can boost exact matches.
  "
  (:require
    [clj-artifact-search.query :as q]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :refer [join]])
  (:import (java.nio.file Paths)
           (org.apache.lucene.store FSDirectory)
           (org.apache.lucene.analysis.standard StandardAnalyzer)
           (org.apache.lucene.index IndexWriterConfig IndexWriterConfig$OpenMode IndexWriter DirectoryReader Term IndexOptions)
           (org.apache.lucene.document Document StringField Field$Store TextField FieldType Field)
           (org.apache.lucene.search IndexSearcher TopDocs ScoreDoc BooleanQuery$Builder TermQuery BooleanClause$Occur Query BoostQuery PrefixQuery RegexpQuery MatchNoDocsQuery)
           (org.apache.lucene.queryparser.classic QueryParser)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.analysis CharArraySet)
           (org.apache.lucene.analysis.miscellaneous PerFieldAnalyzerWrapper)
           (org.apache.lucene.analysis.core StopAnalyzer)
           (java.util.zip GZIPInputStream)))

(defn load-artifacts*
  []
  (with-open [in (io/reader (GZIPInputStream. (io/input-stream "https://clojars.org/repo/feed.clj.gz")))]
    (doall (map edn/read-string (line-seq in)))))

(def load-artifacts (memoize load-artifacts*))

(defn ^String artifact->id [{:keys [artifact-id group-id]}]
  (str group-id ":" artifact-id))

(defn unsearchable-stored-field [^String name ^String value]
  (let [type (doto (FieldType.)
               (.setOmitNorms true)
               (.setIndexOptions (IndexOptions/NONE))
               (.setTokenized false)
               (.setStored true)
               (.freeze))]
    (Field.
      name
      value
      type)))


(defn add-versions [doc versions]
  (run!
    #(.add doc
           (unsearchable-stored-field "versions" %))
    versions))

(defn ^Iterable artifact->doc
  [{:keys [^String artifact-id
           ^String group-id
           ^String description
           versions]
    :or {description ""}
    :as artifact}]
  (doto (Document.)
    ;; *StringField* is indexed but not tokenized, term freq. or positional info not indexed
    ;; id: We need a unique identifier for each doc so that we can use updateDocument
    (.add (StringField. "id" (artifact->id artifact) Field$Store/YES))
    (.add (TextField. "artifact-id" artifact-id Field$Store/YES))
    ;; Keep also un-tokenized version of the id for RegExp searches (Better to replace with
    ;; a custom tokenizer that produces both the original + individual tokens)
    (.add (StringField. "artifact-id-raw" artifact-id Field$Store/YES))
    (.add (TextField. "group-id" group-id Field$Store/YES))
    (.add (StringField. "group-id-raw" group-id Field$Store/YES))
    (.add (TextField. "description" description Field$Store/YES))
    (add-versions versions)))

(defn index-artifacts [^IndexWriter idx-writer artifacts create?]
  (run!
    (fn [artifact]
      (if create?
        (.addDocument idx-writer (artifact->doc artifact))
        (.updateDocument
          idx-writer
          (Term. "id" (artifact->id artifact))
          (artifact->doc artifact))))

    artifacts))

(defn ^FSDirectory fsdir []
  (let [^String index-path "index"]
    (FSDirectory/open (Paths/get index-path
                                 (into-array String nil)))))

(defn mk-indexing-analyzer []
  (PerFieldAnalyzerWrapper.
    (StandardAnalyzer.)
    ;; StandardAnalyzer does not break at . as in 'org.clojure':
    {"group-id" (StopAnalyzer. (CharArraySet. ["." "-"] true))}))

(defn index! []
  (let [{:keys [create?]} {:create? true}
        analyzer (mk-indexing-analyzer)
        iw-cfg   (doto (IndexWriterConfig. analyzer)
                   (.setOpenMode (if create?
                                   IndexWriterConfig$OpenMode/CREATE
                                   IndexWriterConfig$OpenMode/CREATE_OR_APPEND))
                   ;; BEWARE Increase also heap size: -Xmx512m or -Xmx1g
                   (.setRAMBufferSizeMB 256.0))
        idx-dir  (fsdir)]
    (with-open [idx-writer (IndexWriter. idx-dir iw-cfg)]
      (time
        (index-artifacts idx-writer (load-artifacts) create?)))))

;;###################################################################### Search

(defn exact-search
  "Example."
  [query-str]
  (with-open [reader (DirectoryReader/open (fsdir))]
    (let [page-size         30
          searcher         (IndexSearcher. reader)
          analyzer         (StandardAnalyzer.)
          parser           (QueryParser. "artifact-id" analyzer)
          query            (.parse parser query-str)
          ^TopDocs topdocs (.search searcher query page-size)
          hits-cnt         (-> topdocs (.-totalHits) (.-value))
          hits             (.scoreDocs topdocs)
          results          (doall
                             (map
                               (fn [^ScoreDoc h]
                                 (-> (.doc searcher (.-doc h))
                                     (.get "artifact-id")))
                               hits))]
      {:total-hits hits-cnt :results results :page-size page-size})))

(defn search [query-in]
  ;; TODO If no matches, try again but using OR instead of AND in multi-tokens->query?
  (with-open [reader (DirectoryReader/open (fsdir))]
    (let [page-size        30
          searcher         (IndexSearcher. reader)
          analyzer         (StandardAnalyzer.)
          ;parser           (QueryParser. "artifact-id" analyzer)
          ;query            (.parse parser query-str)
          ^Query query      (if (instance? Query query-in)
                              query-in
                              (q/parse-query query-in))
          ^TopDocs topdocs (.search searcher query page-size)
          hits-cnt         (-> topdocs (.-totalHits) (.-value))
          hits             (.scoreDocs topdocs)
          results          (doall
                             (map
                               (fn [^ScoreDoc h]
                                 (-> (.doc searcher (.-doc h))
                                     (.get "id")))
                               hits))]
      {:total-hits hits-cnt :results results :page-size page-size})))

(comment

  (RegexpQuery. (Term. "artifact-id", "re-frame"))

  (def analyzers
    {:stop   (StopAnalyzer. (CharArraySet. ["." "-"] true))
     :standard (StandardAnalyzer.)})

  (Automata/makeString "re")


  nil)