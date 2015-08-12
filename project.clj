(defproject curiosity.utils "0.6.0"
  :description "Misc clojure functions"
  :url "https://github.com/CuriosityApp/curiosity.utils"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins  [[codox "0.8.10"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/schema "0.4.3"]
                 [prismatic/plumbing "0.4.4"]
                 [potemkin "0.4.1"]
                 [com.taoensso/encore "2.4.2"]]
  :codox  {:src-dir-uri "http://github.com/curiosityapp/curiosity.utils/blob/master/"
           :src-linenum-anchor-prefix "L"})

