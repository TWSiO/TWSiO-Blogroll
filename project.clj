(defproject blogroll-script "0.1.0-SNAPSHOT"
  :description "A script that I use to update my site's blogroll."
  :url "http://github.com/TWSiO/blogroll-script"
  :license {:name "GNU Affero General Public License"
            :url "https://www.gnu.org/licenses/agpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-http "3.12.3"]
                 [com.github.kyleburton/clj-xpath "1.4.13"]
                 [dev.weavejester/medley "1.7.0"]
                 [enlive "1.1.6"]
                 [org.clojars.twsio/neocities-clj "0.1.0"]
                 ]
  :main blogroll-script.core
  :repl-options {:init-ns blogroll-script.core})
