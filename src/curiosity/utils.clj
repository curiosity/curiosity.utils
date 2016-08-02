(ns curiosity.utils
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [plumbing.core :as plumbing]
            [taoensso.encore :as encore]
            potemkin
            [clojure.string :as str]))

(encore/defalias defalias encore/defalias)
(defalias have? encore/have?)
(defalias have! encore/have!)
(defalias have!? encore/have!?)
(defalias have encore/have)
(defalias have-in encore/have-in)
(defalias have-in! encore/have-in!)
(defalias if-lets encore/if-lets)
(defalias when-lets encore/when-lets)
(defalias unify-gensyms potemkin/unify-gensyms)

(defn make-merger
  "Wraps an f that takes a map and returns the result
   of merging that map with the result of the map applied
   to the f."
  [f]
  (fn [doc]
    (merge doc (f doc))))

(defn pprint-str
  "Pretty Prints x, returning it as a string"
  [x]
  (with-out-str
    (pprint x)))

(defn any?
  "Returns the first logical true value or nil.
  This is essentially clojure.core/or but apply-able"
  ([] nil)
  ([x] x)
  ([x y] (or x y))
  ([x y & more] (or x y (first (filter boolean more)))))

(defn java-map?
  "Returns true if coll implements java.util.Map"
  {:static true}
  [coll]
  (instance? java.util.Map coll))

(defn seq-deep-merge-fn
  [final-decider x y]
  (when (or x y)
    (let [?same (= (type x) (type y))
          cx (coll? x)
          cy (coll? y)
          ?colls (and cx (= cx cy))]
      (cond
        (and ?same (java-map? x)) (merge-with (partial seq-deep-merge-fn final-decider) x y)
        (and ?same (set? x)) (into (set x) (set y))
        (and ?same (sequential? x)) (vec (into (set x) (set y)))
        ?colls (into x y)
        cx (into x [y])
        cy (into y [x])
        :else (final-decider x y)))))

(def deep-merge
  "Recursively merge maps, treating non-map child sequences as unordered deduped collections represented as a vector (if all inputs were not sets) or a set (if all inputs were sets). When types of inputs don't match, but one of the inputs is a collection, write the non-collection into the collection using into. When all else fails, take the right-most value."
  (partial merge-with (partial seq-deep-merge-fn (fn [x y] y))))


(def ^:dynamic
  *bool-or-nil-logger*
  "Fn that takes an exception and presumably logs it"
  prn)

(defn bool-or-nil-fn
  "True if f returns boolean true.
   False if f returns boolean false.
   nil if f throws.
   f is a thunk
  Logs exceptions via *bool-or-nil-logger* (default: prn)"
  ([f]
   (try
     (boolean (f))
     (catch Exception e
       (*bool-or-nil-logger* e)
       nil))))

(defmacro bool-or-nil
  "True if body evaluates to boolean true.
  False if body evaluates to boolean true.
  nil if body throws.
  body is run in an implicit do
  Logs exceptions via *bool-or-nil-logger* (default: prn)"
  [& body]
  `(let [f# (fn [] (do ~@body))]
     (bool-or-nil-fn f#)))

(def deref-at-str
  "Returns the deref'd value of the var named by str"
  (comp deref resolve symbol))

;; Shorthand for common use-cases of if-let/when-let
;;
(defmacro when-do
  "When value is logical true, pass it to falsey"
  [value f]
  `(when ~value
     (~f ~value)))

(defmacro if-do
  "Evaluate value and pass it to truthy/falsey"
  ([value truthy]
   (when-do value truthy))
  ([value truthy falsey]
   `(if ~value
      (~truthy ~value)
      (~falsey ~value))))

(defmacro guard
  "Guards the first argument of the first form of body by evaluating the argument and then testing
  with when. If the argument evaluates falsey, return nil, otherwise, fully evaluate body. The tested
  argument will only be evaluated once.

  This is like when-do, but much cleaner for interop calls"
  [& body]
  (let [x (gensym)
        first-form (first body)
        replaced (second first-form)
        remaining-form (nnext first-form)
        new-form (cons (first first-form) (cons x remaining-form))
        new-body (cons new-form (next body))]
    `(let [~x ~replaced]
       (when ~x
         ~@new-body))))

(defmacro if-seq-let
  "if expr in binding is a seq, run then else else (default nil)
   Note: sym is bound to the result of expr even in the else scope (not necessarily nil/false)"
  {:arglists '([[sym expr] then else]
               [[sym expr] then])}
  [[sym expr] & body]
  (let [then (first body)
        else (second body)]
    `(let [~sym ~expr]
           (if (seq ~sym)
             ~then
             ~else))))

(defmacro when-seq-let
  "when expr is a seq, run the body in an implicit do, else nil"
  {:arglists '([[sym expr] & body])}
  [a-binding & body]
  `(if-seq-let ~a-binding (do ~@body)))

;; from https://github.com/flatland/useful/blob/138cfa0a5a392533b8de9a950faae44408362297/src/flatland/useful/experimental.clj#L31
;; Copyright 2013 Alan Malloy, licensed under the Eclipse Public License 1.0 (same as curiosity.utils)
(defmacro cond-let
  "An implementation of cond-let that is as similar as possible to if-let. Takes multiple
  test-binding/then-form pairs and evalutes the form if the binding is true. Also supports
  :else in the place of test-binding and always evaluates the form in that case.
 
  Example:
   (cond-let [b (bar 1 2 3)] (println :bar b)
             [f (foo 3 4 5)] (println :foo f)
             [b (baz 6 7 8)] (println :baz b)
             :else           (println :no-luck))

  from https://github.com/flatland/useful/blob/138cfa0a5a392533b8de9a950faae44408362297/src/flatland/useful/experimental.clj#L31
  "
  [test-binding then-form & more]
  (let [test-binding (if (= :else test-binding) `[t# true] test-binding)
        else-form    (when (seq more) `(cond-let ~@more))]
    `(if-let ~test-binding
       ~then-form
       ~else-form)))

;; defining forms
;;
;; We like the look of the arglists like they are the args to a defn better than explicit metadata
(defmacro defcomp
  "Define function composition at var vname, providing appropriate docstrings and arglists"
  [vname docstring arglists & fns]
  (let [args (if (vector? arglists)
               (list arglists)
               arglists)
        sym (with-meta vname (assoc (meta vname) :arglists `(do '~args)))
        _ (prn (type sym))]
    `(def ~sym
       ~docstring
       (comp ~@fns))))

;; misc utility
;;
(def path-split
  "Split on / or nil"
  #(when (string? %)
     (string/split % #"\/")))

(def csv-split
  "Split on commas or nil. For more complicated use cases, see clojure.data.csv"
  #(when (string? %)
     (string/split % #",")))

(defn assoc'
  "calls assoc or assoc-in depending on whether a vector of keys was supplied."
  [m keyish value]
  ((if (vector? keyish)
    assoc-in
    assoc) m keyish value))

(defn dissoc'
  "calls dissoc or dissoc-in depending on whether a vector of keys was supplied."
  [m keyish]
  ((if (vector? keyish)
     plumbing/dissoc-in
     dissoc) m keyish))

(defn get'
  "calls get or get-in depending on whether a vector of keys was supplied."
  ([m keyish]
   (get' m keyish nil))
  ([m keyish default]
   ((if (vector? keyish)
      get-in
      get) m keyish default)))

(defn update'
  "calls update or update-in depending on whether a vector of keys was supplied."
  [m keyish f]
  ((if (vector? keyish)
    update-in
    update) m keyish f))

(defn move-key
  "Renames the k1 to k2 in map m, optionally applying t to the value of k1 first"
  ([m k1 k2]
   (move-key m k1 k2 identity))
  ([m k1 k2 t]
   (assoc' (dissoc' m k1) k2 (t (get' m k1)))))

(defn select-values
  "Returns a seq of the values specified by ks in m. Uses get' so supports arbitrary nested
   lookups"
  [m ks]
  (map (partial get' m) ks))

(defn ignore-args
  "Returns an f that ignores the first n arguments."
  [n f]
  (fn [& xs]
    (apply f (drop n xs))))

(defmacro sim-starity
  "Takes a function which is not star arity, and simulates star arity through
   repeated invocations of f"
  [f]
  `(fn [x# & ys#]
     (loop [r# x# args# ys#]
       (if (seq args#)
         (recur (apply ~f r# (first args#))
                (rest args#))
         r#))))

(defn slice
  "Given a keyspec, return a map with just that keyspec"
  [m k]
  (assoc' nil k (get' m k)))

(defn filter-slices
  "Return just the slices of the map you want"
  [m & keyspecs]
  (apply merge nil (map (partial slice m) keyspecs)))

(defn slice-n-rename
  "Given a map and a vector of [from-ks to-ksk] keyspecs, slice
   from the from-keyspecs, renaming them to to-keyspecs and return
   the resulting map"
  [m & keyspecs]
  (apply (sim-starity move-key)
         (apply filter-slices m (map first keyspecs))
         keyspecs))

(defn flatten1
  "Flatten 1 level. Items in coll that aren't sequential are wrapped in a vec"
  [coll]
  (mapcat #(if (sequential? %) % [%]) coll))


(defn snake-case
  "Foo-bar -> foo_bar"
  [s]
  (guard (str/replace s "-" "_"))
  (-> s str/lower-case (str/replace "-" "_")))

(defn kebob-case
  "Foo_Bar -> foo-bar"
  [s]
  (-> s str/lower-case (str/replace "_" "-")))

(defn index-exclude [r ex]
  "Take all indices execpted ex"
  (filter #(not (ex %)) (range r)))


(defn dissoc-idx [v & idxs]
  "Remove idxs from v"
  (map v (index-exclude (count v) (into #{} idxs))))

(defmacro forv
  "Like `clojure.core/for`, but forces immediately into a vector"
  [bindings & body]
  `(->> (for ~bindings (do ~@body))
        (into [])))

(defn positions
  "Returns a sequence of indexes for items in a seq that match pred.
   If you pass a set, it works like (.indexOf v) on vectors, but returns all matches"
  [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))
