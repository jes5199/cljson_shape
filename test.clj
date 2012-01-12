(do
 (clojure.core/println "let's begin")
 (clojure.core/declare tuple_parameters)
 (clojure.core/declare builtin_type_with_mandatory_parameters)
 (clojure.core/declare definition_pair)
 (clojure.core/declare array_parameters)
 (clojure.core/declare builtin_type_with_optional_parameters)
 (clojure.core/declare comment)
 (clojure.core/declare optional_definition)
 (clojure.core/declare integer_parameters)
 (clojure.core/declare json_shape)
 (clojure.core/declare definition_atom)
 (clojure.core/declare dictionary_parameters)
 (clojure.core/declare string_parameters)
 (clojure.core/declare builtin_type)
 (clojure.core/declare object_parameters)
 (clojure.core/declare enum_parameters)
 (clojure.core/declare definition)
 (clojure.core/declare definition_singleton)
 (clojure.core/declare optional_definitions)
 (clojure.core/declare restrict_parameters)
 (clojure.core/declare builtin_type_without_parameters)
 (clojure.core/declare number_parameters)
 (clojure.core/declare custom_type)
 (clojure.core/declare either_parameters)
 (clojure.core/defn
  tuple_parameters
  [& _]
  (object {:members {:elements (array {:contents definition})}}))
 (clojure.core/defn
  builtin_type_with_mandatory_parameters
  [& _]
  (enum
   {:values
    ["literal" "optional" "nullable" "enum" "tuple" "either"]}))
 (clojure.core/defn
  definition_pair
  [& _]
  (either
   {:choices
    ((tuple {:elements (custom_type (literal {}) comment)})
     (tuple
      {:elements
       (builtin_type_without_parameters (literal {}) comment)})
     (tuple {:elements ((literal "literal") anything comment)})
     (tuple {:elements ((literal "optional") definition comment)})
     (tuple {:elements ((literal "nullable") definition comment)})
     (tuple {:elements ((literal "string") string_parameters comment)})
     (tuple {:elements ((literal "number") number_parameters comment)})
     (tuple
      {:elements ((literal "integer") integer_parameters comment)})
     (tuple {:elements ((literal "array") array_parameters comment)})
     (tuple {:elements ((literal "object") object_parameters comment)})
     (tuple
      {:elements
       ((literal "dictionary") dictionary_parameters comment)})
     (tuple
      {:elements ((literal "restrict") restrict_parameters comment)})
     (tuple {:elements ((literal "enum") enum_parameters comment)})
     (tuple {:elements ((literal "tuple") tuple_parameters comment)})
     (tuple
      {:elements ((literal "either") either_parameters comment)}))}))
 (clojure.core/defn
  array_parameters
  [& _]
  (object
   {:members
    {:contents optional_definition, :length optional_definition}}))
 (clojure.core/defn
  builtin_type_with_optional_parameters
  [& _]
  (enum
   {:values
    ["string"
     "number"
     "integer"
     "array"
     "object"
     "dictionary"
     "restrict"]}))
 (clojure.core/defn comment [& _] (optional string))
 (clojure.core/defn optional_definition [& _] (optional definition))
 (clojure.core/defn
  integer_parameters
  [& _]
  (object
   {:members {:min (optional integer), :max (optional integer)}}))
 (clojure.core/defn
  json_shape
  [& _]
  (dictionary {:contents definition}))
 (clojure.core/defn
  definition_atom
  [& _]
  (either
   {:choices
    (custom_type
     builtin_type_with_optional_parameters
     builtin_type_without_parameters)}))
 (clojure.core/defn
  dictionary_parameters
  [& _]
  (object
   {:members
    {:keys (optional string), :contents optional_definition}}))
 (clojure.core/defn
  string_parameters
  [& _]
  (object {:members {:matches (optional string)}}))
 (clojure.core/defn
  builtin_type
  [& _]
  (enum
   {:values
    ["string"
     "number"
     "boolean"
     "null"
     "undefined"
     "array"
     "object"
     "anything"
     "literal"
     "optional"
     "nullable"
     "integer"
     "enum"
     "tuple"
     "dictionary"
     "either"
     "restrict"]}))
 (clojure.core/defn
  object_parameters
  [& _]
  (object
   {:members
    {:members (optional (dictionary {:contents definition})),
     :allow_extra (optional boolean),
     :allow_missing (optional boolean)}}))
 (clojure.core/defn
  enum_parameters
  [& _]
  (object {:members {:values (array {:contents anything})}}))
 (clojure.core/defn
  definition
  [& _]
  (either
   {:choices (definition_atom definition_singleton definition_pair)}))
 (clojure.core/defn
  definition_singleton
  [& _]
  (tuple {:elements (definition_atom)}))
 (clojure.core/defn
  optional_definitions
  [& _]
  (optional (array {:contents definition})))
 (clojure.core/defn
  restrict_parameters
  [& _]
  (object
   {:members
    {:require optional_definitions, :reject optional_definitions}}))
 (clojure.core/defn
  builtin_type_without_parameters
  [& _]
  (enum {:values ["boolean" "null" "undefined" "anything"]}))
 (clojure.core/defn
  number_parameters
  [& _]
  (object {:members {:min (optional number), :max (optional number)}}))
 (clojure.core/defn
  custom_type
  [& _]
  (restrict {:require (string), :reject (builtin_type)}))
 (clojure.core/defn
  either_parameters
  [& _]
  (object {:members {:choices (array {:contents definition})}}))
 ((json_shape)
  {:tuple_parameters
   ["object"
    {:members {:elements ["array" {:contents "definition"}]}}],
   :builtin_type_with_mandatory_parameters
   ["enum"
    {:values
     ["literal" "optional" "nullable" "enum" "tuple" "either"]}],
   :definition_pair
   ["either"
    {:choices
     [["tuple" {:elements ["custom_type" ["literal" {}] "comment"]}]
      ["tuple"
       {:elements
        ["builtin_type_without_parameters" ["literal" {}] "comment"]}]
      ["tuple"
       {:elements [["literal" "literal"] "anything" "comment"]}]
      ["tuple"
       {:elements [["literal" "optional"] "definition" "comment"]}]
      ["tuple"
       {:elements [["literal" "nullable"] "definition" "comment"]}]
      ["tuple"
       {:elements
        [["literal" "string"] "string_parameters" "comment"]}]
      ["tuple"
       {:elements
        [["literal" "number"] "number_parameters" "comment"]}]
      ["tuple"
       {:elements
        [["literal" "integer"] "integer_parameters" "comment"]}]
      ["tuple"
       {:elements [["literal" "array"] "array_parameters" "comment"]}]
      ["tuple"
       {:elements
        [["literal" "object"] "object_parameters" "comment"]}]
      ["tuple"
       {:elements
        [["literal" "dictionary"] "dictionary_parameters" "comment"]}]
      ["tuple"
       {:elements
        [["literal" "restrict"] "restrict_parameters" "comment"]}]
      ["tuple"
       {:elements [["literal" "enum"] "enum_parameters" "comment"]}]
      ["tuple"
       {:elements [["literal" "tuple"] "tuple_parameters" "comment"]}]
      ["tuple"
       {:elements
        [["literal" "either"] "either_parameters" "comment"]}]]}],
   :array_parameters
   ["object"
    {:members
     {:contents "optional_definition",
      :length "optional_definition"}}],
   :builtin_type_with_optional_parameters
   ["enum"
    {:values
     ["string"
      "number"
      "integer"
      "array"
      "object"
      "dictionary"
      "restrict"]}],
   :comment ["optional" "string"],
   :optional_definition ["optional" "definition"],
   :integer_parameters
   ["object"
    {:members
     {:min ["optional" "integer"], :max ["optional" "integer"]}}],
   :json_shape
   ["dictionary"
    {:contents "definition"}
    "A json_shape schema object"],
   :definition_atom
   ["either"
    {:choices
     ["custom_type"
      "builtin_type_with_optional_parameters"
      "builtin_type_without_parameters"]}],
   :dictionary_parameters
   ["object"
    {:members
     {:keys ["optional" "string"], :contents "optional_definition"}}],
   :string_parameters
   ["object" {:members {:matches ["optional" "string"]}}],
   :builtin_type
   ["enum"
    {:values
     ["string"
      "number"
      "boolean"
      "null"
      "undefined"
      "array"
      "object"
      "anything"
      "literal"
      "optional"
      "nullable"
      "integer"
      "enum"
      "tuple"
      "dictionary"
      "either"
      "restrict"]}],
   :object_parameters
   ["object"
    {:members
     {:members ["optional" ["dictionary" {:contents "definition"}]],
      :allow_extra ["optional" "boolean"],
      :allow_missing ["optional" "boolean"]}}],
   :enum_parameters
   ["object" {:members {:values ["array" {:contents "anything"}]}}],
   :definition
   ["either"
    {:choices
     ["definition_atom" "definition_singleton" "definition_pair"]}],
   :definition_singleton ["tuple" {:elements ["definition_atom"]}],
   :optional_definitions
   ["optional" ["array" {:contents "definition"}]],
   :restrict_parameters
   ["object"
    {:members
     {:require "optional_definitions",
      :reject "optional_definitions"}}],
   :builtin_type_without_parameters
   ["enum" {:values ["boolean" "null" "undefined" "anything"]}],
   :number_parameters
   ["object"
    {:members
     {:min ["optional" "number"], :max ["optional" "number"]}}],
   :custom_type
   ["restrict" {:require ["string"], :reject ["builtin_type"]}],
   :either_parameters
   ["object"
    {:members {:choices ["array" {:contents "definition"}]}}]}))
let's begin
(#<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@26114629> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@38154145> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@7b7035c6> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@3da997a> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@4921a90> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@140de648> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@1c898b41> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@a15670a> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@31fc6b2> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@1b2dd1b8> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@5d2394f8> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@4b09558d> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@51cfc277> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@1ee83c97> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@20e1ed5b> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@50a6023a> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@42dc5733> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@fe0f790> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@7a9d1714> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@5f5660ef> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@1ff61bcf> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@569bc36d> #<cljson_shape$eval1864$either__1866$check__1869 cljson_shape$eval1864$either__1866$check__1869@c3b5587>)
