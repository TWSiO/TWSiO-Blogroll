(ns blogroll-script.core
  (:gen-class
    :methods [^:static [handler [Object] String]]
    )
  (:require [clj-http.client :as http]
            [clojure.data.xml :as xml]
            [clj-xpath.core :as xpath]
            [medley.core :as m]
            [net.cgrand.enlive-html :as html]
            [neocities-clj.core :as neo]
    ))

(defn get-body
  [url]
  (as-> url X
    (http/get X)
    (:body X)
    ))

(def secret-id "neocities")

(defn get-api-key
  []
  (let
    [aws-token (System/getenv "AWS_SESSION_TOKEN")
     headers {"X-Aws-Parameters-Secrets-Token" aws-token}
     secrets-path (str "/secretsmanager/get?secretId=" secret-id)
     secrets-endpoint (str "http://localhost:"
                           (System/getenv "PARAMETERS_SECRETS_EXTENSION_HTTP_PORT")
                           secrets-path
                           )
     ]
     (as-> secrets-endpoint X
       (http/get X {:headers headers})
       (:body X)
    )))

(defn parse-atom-entry
  [entry]
  (let [subtags @(:children entry)
        subtag-get (fn [tag-name subtag] (as-> subtag X
               (filter #(= tag-name (:tag %)) X)
               (first X)))
        subtag-text #(:text (subtag-get %1 %2))
        dt-format java.time.format.DateTimeFormatter/ISO_OFFSET_DATE_TIME

        date (java.time.LocalDateTime/parse
               (subtag-text :updated subtags)
               dt-format)
        ]
    {:date date
     :link (:href (:attrs (subtag-get :link subtags)))
     :title (subtag-text :title subtags)
     }
   ))

(defn parse-atom
  [atom-string]
  (let [entries (xpath/$x "/feed/entry" atom-string)
        date-titles (map parse-atom-entry entries)
        ]
    (sort-by :date #(- (compare %1 %2)) date-titles)
   ))

(defn parse-rss-items
  [item]
  (let [subtags @(:children item)
        subtag-get (fn [tag-name subtag] (as-> subtag X
               (filter #(= tag-name (:tag %)) X)
               (first X)))
        subtag-text #(:text (subtag-get %1 %2))
        dt-format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME

        date (java.time.LocalDateTime/parse
               (subtag-text :pubDate subtags)
               dt-format)
        ]
    {:date date
     :link (subtag-text :link subtags)
     :title (subtag-text :title subtags)
     }
    ))

(defn parse-rss
  [rss-string]
  (let [items (xpath/$x "/rss/channel/item" rss-string)
        parsed-items (map parse-rss-items items)
        ]
    (sort-by :date #(- (compare %1 %2)) parsed-items)
    ))

(def complete-atom (comp parse-atom get-body))

(def complete-rss (comp parse-rss get-body))

(defn jvns
  ""
  []
  (->> "https://jvns.ca/atom.xml" complete-atom))

(defn computer-things
  ""
  []
  (->> "https://buttondown.email/hillelwayne/rss" complete-rss))

(defn all-feeds
  []
  {:jvns (jvns)
   :computer-things (computer-things)
   })

(defn combined-feeds
  [feeds]
  (let [feeds (vals feeds)
        everything (flatten feeds)
        ]
    (sort-by :date #(- (compare %1 %2)) everything)
    )
  )

(defn feeds-latest
  [feeds]
  (m/map-vals first feeds)
  )

(def twsio-url "https://thiswebsiteis.online")

(def twsio-blogroll (str twsio-url "/sites-i-follow.html"))

(defn get-home
  []
  (get-body twsio-url)
  )

(defn get-blogroll
  []
  (get-body twsio-blogroll)
  )

(defn latest-feeds-html-reducer
  [aggr post]
  (let [date (:date post)
        iso-format java.time.format.DateTimeFormatter/ISO_LOCAL_DATE
        foo (println post)
        li (str
             "<li>"
             "<time datetime=\"" (.toString date) "\">" (.format date iso-format) "</time>"
             "<h3><a href\"" (:link post) "\">" (:title post) "</a></h3"
             "</li>"
             )
        ]
    (str aggr li)
    ))

(defn latest-feeds-html
  [posts]
  (str
    "<ol class=\"blogroll-feed\">"
    (reduce latest-feeds-html-reducer "" posts)
    "</ol>"
    ))

(defn home-feeds
  [html feeds]
  (let [home-template (html/html-resource (java.io.StringReader. html))
        newest-three (take 3 (combined-feeds feeds))
        feeds-html (latest-feeds-html newest-three)
        transformed (html/at home-template [:#blogroll-feed] (html/html-content feeds-html))
        ]
    (apply str (html/emit* transformed))
    ))

(def keyword-id
  {:jvns :#jvns
   :computer-things :#computer-things
   })

(defn blogroll-reducer
  [template [id {:keys [date link title]}]]
  (let [iso-format java.time.format.DateTimeFormatter/ISO_LOCAL_DATE
        newest-html (str
                      "<article id=\"" (subs (name id) 1) "\">"
                      "<h4>Latest Post</h4>"
                      "<h5><a href\"" link "\">" title "</a></h5"
                      "<time datetime=\"" (.toString date) "\">" (.format date iso-format) "</time>"
                      "</article>"
                      )
        ]
  (html/at template [id] (html/html-content newest-html))))

(defn blogroll-feeds
  [html feeds]
  (let [blogroll-template (html/html-resource (java.io.StringReader. html))
        latest-feeds (feeds-latest feeds)
        match-id-post (fn [[site id]] [id (site latest-feeds)])
        id-post (map match-id-post keyword-id)
        ;one (println "keyword-id" keyword-id)
        ;two (println "latest-feeds" latest-feeds)
        ;foo (println "blogroll-template" blogroll-template)
        ;bar (println "id-post" id-post)
        template (reduce blogroll-reducer blogroll-template id-post)
        ]
    ))

(defn upload-home
  [html api-key]
  (spit "/tmp/home.html" html)
  (neo/upload
    {"/index.html" "/tml/home.html"}
    {:api-key api-key})
  )

(defn upload-blogroll
  [html api-key]
  (spit "/tmp/blogroll.html" html)
  (neo/upload
    {"/sites-i-follow.html" "/tmp/blogroll.html"}
    {:api-key api-key})
  )

(defn -main
  "For running with the static site generator."
  [& args]
  (let [feeds (all-feeds)
        pre-home-html (get-home)
        home-html (home-feeds pre-home-html feeds)
        pre-blogroll-html (get-blogroll)
        blogroll-html (blogroll-feeds pre-blogroll-html feeds)
        ]
    (println blogroll-html)
    (println home-html)
    ;(println feeds)
    ))

(defn -handler
  [s]
  (let
    [;api-key (get-api-key)
     feeds (all-feeds)
     pre-home-html (get-home)
     home-html (home-feeds pre-home-html feeds)
     pre-blogroll-html (get-blogroll)
     blogroll-html (blogroll-feeds pre-blogroll-html feeds)
     ]
    (str "Test: \n"
         blogroll-html
         "\n###\n"
         home-html)
    ;(println blogroll-html)
    ;(println home-html)
    ;(upload-home home-html api-key)
    ;(upload-blogroll blogroll-html api-key)
    ))
