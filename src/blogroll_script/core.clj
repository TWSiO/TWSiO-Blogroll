(ns blogroll-script.core
  (:gen-class
    :methods [^:static [handler [Object] String]]
    )
  (:require [clj-http.client :as http]
            [clj-xpath.core :as xpath]
            [medley.core :as m]
            [net.cgrand.enlive-html :as html]
            [neocities-clj.core :as neo]
            [cheshire.core :as json]
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
     possible-port (System/getenv "PARAMETERS_SECRETS_EXTENSION_HTTP_PORT")
     port (if (nil? possible-port) 2773 possible-port)
     secrets-endpoint (str "http://localhost:"
                           port
                           secrets-path
                           )
     body (as-> secrets-endpoint X
            (http/get X {:headers headers})
            (:body X))
     parsed (json/parse-string body true)
     ]
    (:SecretString parsed)
     ))

(defn parse-atom-entry
  [entry]
  (let [subtags @(:children entry)
        subtag-get (fn [tag-name subtag]
                     (as-> subtag X
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

(defn pair-up-reducer
  [aggr [site posts]]
  (into
    aggr
    (map (fn [post] [site post]) posts)))

(defn combined-feeds
  [feeds]
  (let [everything (reduce pair-up-reducer [] feeds)
        comparer (fn [left right] (- (compare left right)))
        ]
    (sort-by
      (fn [[_ post]] (:date post))
      comparer
      everything)
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

(def site-names
  {:jvns {:name "Julia Evans" :url "https://jvns.ca/"}
   :computer-things {:name "Hillel Wayne" :url "https://buttondown.email/hillelwayne/"}
   })

(defn latest-feeds-html-reducer
  [aggr [site-id post]]
  (let [date (:date post)
        iso-format java.time.format.DateTimeFormatter/ISO_LOCAL_DATE
        site-info (site-id site-names)
        li (str
             "<li>"
             "<h3><a href=\"" (:url site-info) "\">" (:name site-info) "</a></h3>"
             "<time datetime=\"" (.toString date) "\">" (.format date iso-format) "</time>"
             "<h4><a href=\"" (:link post) "\">" (:title post) "</a></h4>"
             "</li>"
             )
        ]
    (str aggr li)
    ))

(defn latest-feeds-html
  [posts]
  (str
    (reduce latest-feeds-html-reducer "" posts)
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
                      "<h3>Latest Post</h3>"
                      "<time datetime=\"" (.toString date) "\">" (.format date iso-format) "</time>"
                      "<h4><a href=\"" link "\">" title "</a></h4>"
                      )
        ]
  (html/at template [id] (html/html-content newest-html))))

(defn blogroll-feeds
  [html feeds]
  (let [blogroll-template (html/html-resource (java.io.StringReader. html))
        latest-feeds (feeds-latest feeds)
        match-id-post (fn [[site id]] [id (site latest-feeds)])
        id-post (map match-id-post keyword-id)
        template (reduce blogroll-reducer blogroll-template id-post)
        ]
    (apply str (html/emit* template))
    ))

(defn upload-home
  [html api-key]
  (spit "/tmp/home.html" html)
  (neo/upload
    {"/index.html" "/tmp/home.html"}
    :api-key api-key))

(defn upload-blogroll
  [html api-key]
  (spit "/tmp/blogroll.html" html)
  (neo/upload
    {"/sites-i-follow.html" "/tmp/blogroll.html"}
    :api-key api-key))

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
    (neo/upload
      {"/test.txt" "/tmp/test.txt"}
      :api-key "956487786cd7b3584d32f782210e99c0")
    ))

(defn -handler
  [s]
  (let
    [api-key (get-api-key)
     feeds (all-feeds)
     pre-home-html (get-home)
     home-html (home-feeds pre-home-html feeds)
     pre-blogroll-html (get-blogroll)
     blogroll-html (blogroll-feeds pre-blogroll-html feeds)
     ]

    (upload-home home-html api-key)
    (upload-blogroll blogroll-html api-key)
    "Finished"
    ))
