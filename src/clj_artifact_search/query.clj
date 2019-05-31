(ns clj-artifact-search.query
  (:require
    [clojure.string :refer [join]])
  (:import (org.apache.lucene.analysis.standard StandardAnalyzer)
           (java.util.regex Pattern)
           (org.apache.lucene.search BooleanClause$Occur PrefixQuery BoostQuery BooleanQuery$Builder TermQuery MatchNoDocsQuery RegexpQuery Query BooleanClause)
           (org.apache.lucene.index Term)
           (org.apache.lucene.analysis TokenStream)))

(defn tokenize
  ([text] (tokenize (StandardAnalyzer.) text))
  ([analyzer text]
   (let [^TokenStream stream (.tokenStream analyzer "fake-field-name" text)
         attr                (.addAttribute stream org.apache.lucene.analysis.tokenattributes.CharTermAttribute)]
     (.reset stream)
     (take-while
       identity
       (repeatedly
         #(when (.incrementToken stream) (str attr)))))))

(defn raw-field [field]
  (get {:artifact-id :artifact-id-raw
        :group-id :group-id-raw}
       field))
;
;(defn lucene-quote-regexp
;  "Lucene RegExp is only a subset of Java RegExp so we cannot use Pattern/quote.
;   The only character likely to appear in search that we want to escape is '.',
;   used in group names.
;   See http://lucene.apache.org/core/8_0_0/core/org/apache/lucene/util/automaton/RegExp.html
;  "
;  [text]
;  (clojure.string/replace text "." "\\."))
;
;(defn ^String tokens->regexp [tokens]
;  (str
;    (->> tokens
;         (map lucene-quote-regexp)
;         (join "."))
;    ".*"))
;
;(defn ^Query regexp-query [field tokens]
;  (when-let [field (raw-field field)]
;    (RegexpQuery.
;      (term field (tokens->regexp tokens)))))

(defn term [field ^String value]
  (Term. (name field) value))

(defn ^Query boolean-query*
  ([queries] (boolean-query* BooleanClause$Occur/SHOULD queries))
  ([occur queries]
   (let [builder (BooleanQuery$Builder.)]
     (run! (fn [^Query q]
             (when q
               (.add builder q occur)))
           queries)
     (.build builder))))

(defn ^Query boolean-query
  ([occur-or-q & queries]
   (if (instance? BooleanClause$Occur occur-or-q)
     ;; NOTE: Order of subqueries does not matter so it's OK to re-order
     (boolean-query* occur-or-q queries)
     (boolean-query* (conj queries occur-or-q)))))

(defn ^Query multi-tokens->query
  "-> `TermQuery* PrefixQuery`"
  [field tokens]
  (let [exact-qs (map
                   #(TermQuery. (term field %))
                   (butlast tokens))
        prefix-q (PrefixQuery. (term field (last tokens)))]
    (apply boolean-query
           BooleanClause$Occur/MUST
           prefix-q exact-qs)))

(defn single-token->query [field ^String token]
  (PrefixQuery. (term field token)))

(def boosts {:artifact-id 3
             :group-id 2
             :description 1})

(defn tokens->query [field tokens query-text]
  (BoostQuery.
    (if (next tokens)
      (boolean-query
        (multi-tokens->query field tokens)
        ;; Add an exact-match query to boost exact matches so that
        ;; "re-frame" -> "re-frame:re-frame" comes first
        (when-let [field (raw-field field)]
          (TermQuery. (term field query-text))))
      (single-token->query field (first tokens)))
    (get boosts field)))

(defn  ^Query parse-query [^String query-text]
  (let [tokens (tokenize query-text)]
    (->> [:artifact-id :group-id :description]
         (map #(tokens->query % tokens query-text))
         (apply boolean-query))))
