(defproject curiosity.utils "0.9.2"
  :description "Misc clojure functions"
  :url "https://github.com/CuriosityApp/curiosity.utils"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins  [[codox "0.8.10"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [prismatic/schema "1.1.3"]
                 [prismatic/plumbing "0.5.3"]
                 [potemkin "0.4.3"]
                 [com.taoensso/encore "2.88.2"]]
  :codox  {:src-dir-uri "http://github.com/curiosityapp/curiosity.utils/blob/master/"
           :src-linenum-anchor-prefix "L"})

