(defproject cljs-vanilla-react-example "0.0.1"
  :description "React & ClojureScript "
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}
  :signing {:gpg-key "9DD8C3E9"}
  :source-paths ["src/main"]
  :resource-paths ["resources"]
  :plugins [[lein-ancient "0.6.15"]
            [lein-shell "0.5.0"]]
  :profiles {:clj {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :cljs {:dependencies [[thheller/shadow-cljs "2.11.7"]]}}
  :aliases {"watch" ["shell" "./node_modules/.bin/shadow-cljs" "watch" "app"]})
