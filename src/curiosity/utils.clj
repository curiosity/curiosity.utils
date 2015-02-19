(ns curiosity.utils
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string] 
            [plumbing.core :as plumbing]
            [taoensso.encore :as encore]
            potemkin))

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

(def any? 
  "True if anything in the collection is logical true"
  (partial some identity))

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
    plumbing/update) m keyish f))

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
