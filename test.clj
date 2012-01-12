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
  (clojure.core/fn
   [x & [path]]
   ((object
     {:members {:elements (array {:contents (definition nil)})}})
    x
    path)))
 (clojure.core/defn
  builtin_type_with_mandatory_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((enum
     {:values
      ["literal" "optional" "nullable" "enum" "tuple" "either"]})
    x
    path)))
 (clojure.core/defn
  definition_pair
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((either
     {:choices
      [(tuple
        {:elements [(custom_type nil) (literal {}) (comment nil)]})
       (tuple
        {:elements
         [(builtin_type_without_parameters nil)
          (literal {})
          (comment nil)]})
       (tuple
        {:elements [(literal "literal") (anything nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "optional") (definition nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "nullable") (definition nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "string") (string_parameters nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "number") (number_parameters nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "integer") (integer_parameters nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "array") (array_parameters nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "object") (object_parameters nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "dictionary")
          (dictionary_parameters nil)
          (comment nil)]})
       (tuple
        {:elements
         [(literal "restrict")
          (restrict_parameters nil)
          (comment nil)]})
       (tuple
        {:elements
         [(literal "enum") (enum_parameters nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "tuple") (tuple_parameters nil) (comment nil)]})
       (tuple
        {:elements
         [(literal "either")
          (either_parameters nil)
          (comment nil)]})]})
    x
    path)))
 (clojure.core/defn
  array_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object
     {:members
      {:contents (optional_definition nil),
       :length (optional_definition nil)}})
    x
    path)))
 (clojure.core/defn
  builtin_type_with_optional_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((enum
     {:values
      ["string"
       "number"
       "integer"
       "array"
       "object"
       "dictionary"
       "restrict"]})
    x
    path)))
 (clojure.core/defn
  comment
  [& _]
  (clojure.core/fn [x & [path]] ((optional (string nil)) x path)))
 (clojure.core/defn
  optional_definition
  [& _]
  (clojure.core/fn [x & [path]] ((optional (definition nil)) x path)))
 (clojure.core/defn
  integer_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object
     {:members
      {:min (optional (integer nil)), :max (optional (integer nil))}})
    x
    path)))
 (clojure.core/defn
  json_shape
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((dictionary {:contents (definition nil)}) x path)))
 (clojure.core/defn
  definition_atom
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((either
     {:choices
      [(custom_type nil)
       (builtin_type_with_optional_parameters nil)
       (builtin_type_without_parameters nil)]})
    x
    path)))
 (clojure.core/defn
  dictionary_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object
     {:members
      {:keys (optional (string nil)),
       :contents (optional_definition nil)}})
    x
    path)))
 (clojure.core/defn
  string_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object {:members {:matches (optional (string nil))}}) x path)))
 (clojure.core/defn
  builtin_type
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((enum
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
       "restrict"]})
    x
    path)))
 (clojure.core/defn
  object_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object
     {:members
      {:members (optional (dictionary {:contents (definition nil)})),
       :allow_extra (optional (boolean nil)),
       :allow_missing (optional (boolean nil))}})
    x
    path)))
 (clojure.core/defn
  enum_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object {:members {:values (array {:contents (anything nil)})}})
    x
    path)))
 (clojure.core/defn
  definition
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((either
     {:choices
      [(definition_atom nil)
       (definition_singleton nil)
       (definition_pair nil)]})
    x
    path)))
 (clojure.core/defn
  definition_singleton
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((tuple {:elements [(definition_atom nil)]}) x path)))
 (clojure.core/defn
  optional_definitions
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((optional (array {:contents (definition nil)})) x path)))
 (clojure.core/defn
  restrict_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object
     {:members
      {:require (optional_definitions nil),
       :reject (optional_definitions nil)}})
    x
    path)))
 (clojure.core/defn
  builtin_type_without_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((enum {:values ["boolean" "null" "undefined" "anything"]})
    x
    path)))
 (clojure.core/defn
  number_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object
     {:members
      {:min (optional (number nil)), :max (optional (number nil))}})
    x
    path)))
 (clojure.core/defn
  custom_type
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((restrict {:require [(string nil)], :reject [(builtin_type nil)]})
    x
    path)))
 (clojure.core/defn
  either_parameters
  [& _]
  (clojure.core/fn
   [x & [path]]
   ((object {:members {:choices (array {:contents (definition nil)})}})
    x
    path)))
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
