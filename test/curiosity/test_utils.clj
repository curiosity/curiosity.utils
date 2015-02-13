(ns curiosity.test_utils
  (:require [clojure.test :refer :all]
            [curiosity.utils :as utils]))

(deftest get'
  (let [d {:foo {:bar :baz}
           :spam :eggs}]
    (are [keyish r] (= r (utils/get' d keyish))
         :spam :eggs
         [:spam] :eggs
         :foo {:bar :baz}
         [:foo] {:bar :baz}
         [:foo :bar] :baz
         nil nil
         :bar nil
         [:foo :spam] nil
         [:spam :eggs] nil)))

(deftest move-key
  (let [d {:foo {:bar :baz}
           :spam :eggs}]
    (are [from to transform r] (= r (utils/move-key d from to transform))
         :foo :spam identity {:spam{:bar :baz}}
         :foo :spam #(assoc % :dog :bark) {:spam {:bar :baz :dog :bark}}
         [:foo :bar] :spam identity {:spam :baz}
         [:foo :bar] [:foo :baz] identity {:foo {:baz :baz} :spam :eggs}
         [:foo :bar] [:pika :chu :is :awesome] identity {:spam :eggs
                                                         :pika {:chu {:is {:awesome :baz}}}}
         ;; if from doesn't exist and keys resolves to only maps and nils, we use nil
         [:pika :chu] :foo identity {:foo nil :spam :eggs})
    ;; if from doesn't exist and the hierarchy contains a non-map, we'll throw
    (is (thrown? ClassCastException (utils/move-key d [:spam :eggs] :qux identity)))))

(deftest when-do
  (are [v r] (= r (utils/when-do v identity))
       nil nil
       false nil
       true true
       ::success ::success))

(deftest if-do
  (let [truthy #(vector true %)
        falsey #(vector false %)]
    (are [v r] (= r (utils/if-do v truthy falsey))
         nil [false nil]
         false [false false]
         true [true true]
         ::success [true ::success])))

(deftest ignore-args
  (are [n r] (= r (apply (utils/ignore-args n vector) (range 3)))
       0 [0 1 2]
       1 [1 2]
       2 [2]
       3 []))
