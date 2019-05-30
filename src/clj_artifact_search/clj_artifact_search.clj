;; TODO Take download count into account
;; TODO Optimize: try various searches => adjust query type, weights
;; TODO Review, simplify
;; FIXME "re-frame" finds nothing, "frame" does
(ns clj-artifact-search.clj-artifact-search
  "Index and search MAven/Clojars artifacts."
  (:require
    [clojure.edn :as edn])
  (:import (java.nio.file Paths)
           (org.apache.lucene.store FSDirectory)
           (org.apache.lucene.analysis.standard StandardAnalyzer)
           (org.apache.lucene.index IndexWriterConfig IndexWriterConfig$OpenMode IndexWriter DirectoryReader Term IndexOptions)
           (org.apache.lucene.document Document StringField Field$Store TextField FieldType Field)
           (org.apache.lucene.search IndexSearcher TopDocs ScoreDoc BooleanQuery$Builder TermQuery BooleanClause$Occur Query BoostQuery PrefixQuery)
           (org.apache.lucene.queryparser.classic QueryParser)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute)
           (org.apache.lucene.analysis CharArraySet)
           (org.apache.lucene.analysis.miscellaneous PerFieldAnalyzerWrapper)
           (org.apache.lucene.analysis.core StopAnalyzer)))

(defn load-artifacts*
  []
  (->
    (str "[" (slurp "data/feed.clj") "]")
    (edn/read-string)))

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
    ;; StringField is indexed but not tokenized, term freq. or positional info not indexed
    ;; We need a unique identifier for each doc so that we can use updateDocument
    (.add (StringField. "id" (artifact->id artifact) Field$Store/YES))
    (.add (TextField. "artifact-id" artifact-id Field$Store/YES))
    (.add (TextField. "group-id" group-id Field$Store/YES))
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
    (let [page-size         10
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

(defn ^Query ->query [^String query-text]
  (.build
    (doto (BooleanQuery$Builder.)
      (.add (BoostQuery.
              (PrefixQuery. (Term. "artifact-id" query-text))
              4)
            (BooleanClause$Occur/SHOULD))
      (.add (BoostQuery.
              (PrefixQuery. (Term. "group-id" query-text))
              2)
            (BooleanClause$Occur/SHOULD))
      (.add (TermQuery. (Term. "description" query-text))
            (BooleanClause$Occur/SHOULD)))))

;; WANTED:
;; "conc" -> clj-concordion
;; "ring" -> ring etc
;; "rung" -> ring
;; clojure -> clojure, clojurescript
;; nyam -> clj-nyam
;; re-frame -> re-frame
(defn search [query-text]
  ;; TODO 1) search all fields, with weights; 2) search anywhere in the field text
  (with-open [reader (DirectoryReader/open (fsdir))]
    (let [page-size        10
          searcher         (IndexSearcher. reader)
          analyzer         (StandardAnalyzer.)
          ;parser           (QueryParser. "artifact-id" analyzer)
          ;query            (.parse parser query-str)
          query            (->query query-text)
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

  (def analyzers
    {:stop   (StopAnalyzer. (CharArraySet. ["." "-"] true))
     :standard (StandardAnalyzer.)})

  (defn tokenize
    ([text] (tokenize (StandardAnalyzer.)))
    ([analyzer text]
     (let [stream   (.tokenStream analyzer "fake-field-name" text)
           attr     (.addAttribute stream org.apache.lucene.analysis.tokenattributes.CharTermAttribute)]
       (.reset stream)
       (take-while
         identity
         (repeatedly
           #(when (.incrementToken stream) (str attr))))))))