(ns cljson-shape
  ""
  (:use [clojure.contrib.json :only [read-json]]
        [clojure.contrib.duck-streams :only [reader]]
        [clojure.contrib.with-ns :only [with-ns with-temp-ns]]
        [clojure.contrib.pprint]
  )
)

(defn fail-at [path message] (str path message))

(create-ns 'cljson-shape-predicates)

(defmacro def-shape [name body]
  (do
    (println (symbol "cljson-shape-predicates" (str name)))
    `(with-ns ~(find-ns 'cljson-shape-predicates)
      (defn ~name
        [~'& [~'param]]
         (fn [~'x ~'& [~'path]]
           ( do
           (let [
               ~'fail (fn [~'s] (fail-at ~'path ~'s))
               ~'refine (fn [~'ff] (~'ff ~'x ~'path))
               ~'delve (fn [~'index ~'rule ~'val] (~'rule ~'x (concat ~'path "/" ~'index)))
             ]
             ~body
           )
           )
        )
      )
    )
  )
)

(def-shape literal
  (if-not (= param x) (fail "doesn't match"))
)

(def-shape optional
  ( if-not (= x :undefined) (refine param) )
)

(def-shape undefined
  ( if-not (= x :undefined) (fail "is not undefined") )
)

(def-shape anything
  nil
)

(def-shape string
  ( if-not (string? x) (fail "isn't a string") ) ; TODO: matches
)

(def-shape number
  ( or
    ( if-not (number? x)    (fail "isn't a number") )
    ( if-let [max (:max param)] (if (> x max) (fail "is too big"  ) ) )
    ( if-let [min (:min param)] (if (< x max) (fail "is too small") ) )
  )
)

(def-shape integer
  ( or
    ( if-not (integer? x) (fail "isn't an integer") )
    ( refine (cljson-shape-predicates/number param) )
  )
)

(def-shape boolean
  ( if-not (or (true? x) (false? x)) (fail "isn't a boolean") )
)

(def-shape nullable
  ( if-not (nil? x) (fail "isn't null") )
)

(def-shape null
  ( if-not (nil? x) (refine param) )
)

(def-shape object
  ( if-not (map? x) (fail "isn't an object") )
)

(def-shape array
  ( or
    ( if-not (sequential? x) (fail "isn't an array") )
    ; TODO contents
    ; TODO length
  )
)

(def-shape dictionary
  ( or
    ( if-not (map? x) (fail "isn't an object") )
    ( if (:keys param)
      (map
        (fn [key] (delve key (:keys param) keys))
        (keys x)
      )
    )
    ( if (:contents param)
      (map
        (fn [[key value]] (delve key (:contents param) value))
        x
      )
    )
  )
)

(def-shape enum
  ( if-not (reduce #(or %1 (cljson-shape-predicates/literal %2 x)) (:values param)) (fail "isn't in the list") )
)

(def-shape either
  ( if ( every? #(% x) (:choices param)) (fail "isn't in the list") )
)

(def-shape tuple
  ( cljson-shape-predicates/array nil x)
)

(def-shape restrict
  nil
)

(def shape (read-json (reader "json_shape.shape.json")))

(defn symkey [key] (symbol (name key)))
(defn keykey [key] (keyword (name key)))

; merge-with #(%2 %1) ; but doesn't create keys
(defn merge-apply [hash conversions]
  (reduce
    (fn [h k]
      (if (contains? h k)
        (assoc h k ((get conversions k) (get h k)))
        h
      )
    )
    hash
    (keys conversions)
  )
)

(defn fvalues [hash conversions] (merge-apply hash conversions) )

(defn vmap [f hash] (apply array-map (mapcat (fn [[k v]] [k (f v)]) hash )))

(defn functify [thing]
  ( if (sequential? thing)
    ( let [[name param comment] thing]
      ( condp = (keykey name)
        :optional   (list 'optional   (functify param) )
        :nullible   (list 'nullible   (functify param) )
        :either     (list 'either     (fvalues param {:choices  (partial  map functify)}) )
        :tuple      (list 'tuple      (fvalues param {:elements (partial  map functify)}) )
        :array      (list 'tuple      (fvalues param {:contents               functify
                                                      :length                 functify }) )
        :dictionary (list 'dictionary (fvalues param {:contents               functify }) )
        :object     (list 'object     (fvalues param {:members  (partial vmap functify)}) )
        ( list (symkey name) param )
      )
    )
    (functify [thing])
  )
)

(let [validator (concat
  `(do)
  [
    `(println "let's begin")
  ]

  ; iterate over the shape, declaring all the keys
  (map (fn [[key val]] `(declare ~(symkey key) ) ) shape )

  ; iterate over the shape, declaring each definition
  (map (fn [[key val]] `(defn ~(symkey key) [& ~'_] ~(functify val) ) ) shape )

  ; apply the base definition to the data
  [`( (~'json_shape nil) ~shape )]
) ]
  (pprint validator)
  (when-let [error (eval `(with-ns (create-ns (gensym)) (do (clojure.core/refer ~''cljson-shape-predicates) ~validator)))] (println error))
)
