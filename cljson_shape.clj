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
  `(do
    (intern (the-ns ~''cljson-shape-predicates) '~name
      ( fn ~name [& [~'param]]
        (do
          (fn ~'check [~'x & [~'path]]
            (let [
                ~'fail (fn ~'fail [~'s] (fail-at ~'path ~'s))
                ~'refine (fn ~'refine [~'ff] (~'ff ~'x ~'path))
                ~'delve (fn ~'delve [~'index ~'rule ~'val] (~'rule ~'val (concat ~'path "/" ~'index)))
              ]
              (do
                ~body
              )
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
  ( if-not (nil? x) (refine param) )
)

(def-shape null
  ( if-not (nil? x) (fail "isn't null") )
)

(def-shape object
  ( if-not (map? x) (fail "isn't an object") )
)

(def-shape array
  ( or
    ( if-not (sequential? x) (fail "isn't an array") )
    ( if (:contents param)
      ( do
        ( second
          (reduce
            (fn [[n r] value] [ (+ n 1) (or r (delve (str n) (:contents param) value))] )
            [0 false]
            x
          )
        )
      )
    )
    ; TODO contents
    ; TODO length
  )
)

(def-shape dictionary
  ( or
    ( if-not (map? x) (fail "isn't an object") )
    ( if (:keys param)
      (reduce
        (fn [r key] (or r (delve key (:keys param) keys)))
        false
        (keys x)
      )
    )
    ( if (:contents param)
      ( do
        (reduce
          (fn [r [k value]] (or r (delve k (:contents param) value)))
          false
          x
        )
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
  ( and
    ( cljson-shape-predicates/array nil x)
    ( if (:elements param)
      (map (fn [f a] (f a)) (:elements param) (lazy-cat x (repeat :undefined)))
      ; TODO: does the ruby lib support "allow_extra" ?
    )
  )
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

(defn valmap [f hash] (apply array-map (mapcat (fn [[k v]] [k (f v)]) hash )))

(defn vecmap [f ary] (apply vector (map f ary)))

(defn functify [thing]
  ( if (sequential? thing)
    ( let [[name param comment] thing]
      (list (symkey name)
        ( condp = (keykey name)
          :optional   (functify param)
          :nullible   (functify param)
          :either     (fvalues param {:choices  (partial vecmap functify)})
          :tuple      (fvalues param {:elements (partial vecmap functify)})
          :array      (fvalues param {:contents                 functify
                                      :length                   functify })
          :dictionary (fvalues param {:contents                 functify })
          :object     (fvalues param {:members  (partial valmap functify)})
          :restrict   (fvalues param {:require  (partial vecmap functify)
                                      :reject   (partial vecmap functify)})
          param
        )
      )
    )
    (functify [thing])
  )
)

; (clojure.core/refer 'cljson-shape-predicates)
; (print ( (dictionary {:contents (number nil)})
;   {1 1}
; ) )
; (throw ())

( defmacro assert-checks [predicate subject]
  (if (coll? predicate)
    `(
      assert ( = nil (~(seq predicate) ~subject) )
    )
    `(
      assert ( = nil ((~predicate) ~subject) )
    )
  )
)

( defmacro assert-fails [predicate subject]
  (if (coll? predicate)
    `(
      assert ( not= nil (~(seq predicate) ~subject) )
    )
    `(
      assert ( not= nil ((~predicate) ~subject) )
    )
  )
)

( assert-checks cljson-shape-predicates/anything "x" )
( assert-checks cljson-shape-predicates/anything 1 )
( assert-checks cljson-shape-predicates/anything [] )
( assert-checks cljson-shape-predicates/anything {} )

( assert-checks [cljson-shape-predicates/literal {"x" "y"}] {"x" "y"} )
( assert-checks [cljson-shape-predicates/literal "x"] "x" )

( assert-checks [cljson-shape-predicates/literal false] false )
( assert-fails [cljson-shape-predicates/literal false] true )

( assert-fails [cljson-shape-predicates/literal {"x" "y"}] "x" )
( assert-fails [cljson-shape-predicates/literal {"x" "z"}] {"x" "y"} )
( assert-fails [cljson-shape-predicates/literal "1"] 1 )

; the nullable type
( assert-checks [cljson-shape-predicates/nullable (cljson-shape-predicates/string)] "x")
( assert-checks [cljson-shape-predicates/nullable (cljson-shape-predicates/literal {"x" "y"})] {"x" "y"})

( assert-checks [cljson-shape-predicates/nullable (cljson-shape-predicates/string)] nil)
( assert-checks [cljson-shape-predicates/nullable (cljson-shape-predicates/literal {"x" "y"})] nil)

( assert-fails [cljson-shape-predicates/nullable (cljson-shape-predicates/number)] "x")

; the string type
( assert-checks cljson-shape-predicates/string "x")
( assert-fails cljson-shape-predicates/string 1)
( assert-fails cljson-shape-predicates/string {})
( assert-fails cljson-shape-predicates/string nil)
( assert-fails cljson-shape-predicates/string ["a"])
( assert-fails cljson-shape-predicates/string true)
( assert-fails cljson-shape-predicates/string false)

( assert-checks (cljson-shape-predicates/string {}) "x")
( assert-checks (cljson-shape-predicates/string {"matches" "^\\w+;\\w+-\\w+$"}) "my;fancy-string")
; pending ( assert-fails (cljson-shape-predicates/string {"matches" "^\\w+;\\w+-\\w+$"}) "my;fancy-string with.other/characters")

; the array type
( assert-checks (cljson-shape-predicates/array) [1])
( assert-checks (cljson-shape-predicates/array {:contents (cljson-shape-predicates/number nil)}) [1])
( assert-fails (cljson-shape-predicates/array {:contents (cljson-shape-predicates/number nil)}) [[]])

; the either type
( assert-checks (cljson-shape-predicates/either {:choices [(cljson-shape-predicates/array nil) (cljson-shape-predicates/number nil) ]}) [])
( assert-checks (cljson-shape-predicates/either {:choices [(cljson-shape-predicates/array nil) (cljson-shape-predicates/number nil) ]}) 1)


; XXX
( assert-fails cljson-shape-predicates/number [] )

( assert-checks cljson-shape-predicates/number 1 )
( assert-fails cljson-shape-predicates/number {} )
( assert-fails cljson-shape-predicates/number [] )

(let [validator (concat
    `(do)
    [`(println "let's begin")]

    ; iterate over the shape, declaring all the keys
    (map (fn [[key val]] `(declare ~(symkey key) ) ) shape )

    ; iterate over the shape, declaring each definition
    (map (fn [[key val]] `(defn ~(symkey key) [& ~'_] (fn [~'x ~'& [~'path] ] (~(functify val) ~'x ~'path ) ) ) ) shape )

    ; apply the base definition to the data
    [`( (~'json_shape) ~shape )]
  ) ]
  (pprint validator)
  (when-let [error (eval `(with-ns (create-ns (gensym)) (do (clojure.core/refer ~''cljson-shape-predicates) ~validator)))] (println {:error error}))
)
