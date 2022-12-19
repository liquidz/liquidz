(ns build
  (:require
    [babashka.curl :as curl]
    [cheshire.core :as json]
    [clojure.data.xml :as xml]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import
    (java.time
      ZoneId
      ZonedDateTime)
    java.time.format.DateTimeFormatter))

;;;;; Releases {{{

(def base-query
  "
query {
  viewer {
    repositories(first: 100, privacy: PUBLIC, after: <<cursor>>) {
      pageInfo {
        hasNextPage
        endCursor
      }
      nodes {
        name
        description
        url
        releases(orderBy: {field: CREATED_AT, direction: DESC}, first: 1) {
          totalCount
          nodes {
            name
            publishedAt
            url
          }
        }
      }
    }
  }
}")

(defn build-query
  [cursor]
  (let [cursor (if cursor (str "\"" cursor "\"") "null")]
    (str/replace base-query "<<cursor>>" cursor)))

(defn repositories
  [resp]
  (get-in (json/parse-string (:body resp) true)
          [:data :viewer :repositories]))

(defn next-cursor
  [repos]
  (let [page-info (:pageInfo repos)]
    (when (:hasNextPage page-info)
      (:endCursor page-info))))

(defn fetch-by-graphql
  ([oauth-token]
   (fetch-by-graphql oauth-token nil))
  ([oauth-token cursor]
   (curl/post "https://api.github.com/graphql"
              {:headers {"Authorization" (str "Bearer " oauth-token)}
               :body (json/generate-string {:query (build-query cursor)})})))

(defn fetch-all-releases
  [oauth-token]
  (let [releases* (fn [repos]
                    (filter #(> (get-in % [:releases :totalCount]) 0)
                            (:nodes repos)))
        fetch* (fn [cursor]
                 (let [repos (-> (fetch-by-graphql oauth-token cursor)
                                 (repositories))]
                   {:releases (releases* repos)
                    :next-cursor (next-cursor repos)}))]
    (->> (iterate #(when-let [cursor (:next-cursor %)]
                     (fetch* cursor))
                  (fetch* nil))
         (take-while (comp not nil?))
         (mapcat :releases))))

(defn published-at
  [release]
  (get-in release [:releases :nodes 0 :publishedAt]))

(defn recent-releases
  [all-releases num-of-releases]
  (->> all-releases
       (sort-by published-at)
       (reverse)
       (take num-of-releases)))

(defn release->adoc-link
  [release]
  (let [repo-name (:name release)
        node (get-in release [:releases :nodes 0])
        date (-> (published-at release)
                 (str/split #"T")
                 (first))]
    (format "link:%s[%s] - %s"
            (:url node)
            (str repo-name " " (:name node))
            date)))
;; }}}

;;;;; Articles {{{

(def zenn-feed "https://zenn.dev/uochan/feed")

(let [zdt (ZonedDateTime/parse "Tue, 08 Dec 2020 21:33:37 GMT"
                               DateTimeFormatter/RFC_1123_DATE_TIME)]
  (.format zdt DateTimeFormatter/ISO_LOCAL_DATE))

(defn- parse-pub-date
  [pub-date-str]
  (ZonedDateTime/parse pub-date-str
                       DateTimeFormatter/RFC_1123_DATE_TIME))

(defn- zoned-date-time->iso-local-date
  [^ZonedDateTime zdt]
  (.format zdt DateTimeFormatter/ISO_LOCAL_DATE))

(defn- parse-feed-item
  [item]
  (->> (:content item)
       (map (juxt :tag (comp first :content)))
       (into {})))

(defn- load-extra-articles
  []
  (edn/read-string
    (slurp "./extra-articles.edn")))

(defn fetch-all-articles
  []
  (->> (concat
         (->> (io/input-stream zenn-feed)
              (xml/parse)
              (xml-seq)
              (filter (comp #{:item} :tag))
              (map parse-feed-item))
         (load-extra-articles))
       (map #(update % :pubDate parse-pub-date))
       (sort-by :pubDate)
       (reverse)))

;; }}}

(defn build-readme
  [{:keys [all-releases all-articles]}]
  (let [template (slurp "README.tmpl")
        releases (recent-releases all-releases 7)
        links (->> (map release->adoc-link releases)
                   (map #(str "- " %))
                   (str/join "\n"))
        articles (->> all-articles
                      (take 5)
                      (map #(format "- link:%s[%s] - %s"
                                    (:link %)
                                    (:title %)
                                    (zoned-date-time->iso-local-date (:pubDate %))))
                      (str/join "\n"))
        readme (-> template
                   (str/replace "<<links>>" links)
                   (str/replace "<<articles>>" articles))]
    (spit "README.adoc" readme)))

(defn -main
  [& _]
  (let [oauth-token (System/getenv "README_TOKEN")
        all-releases (fetch-all-releases oauth-token)
        all-articles (fetch-all-articles)]
    (build-readme {:all-releases all-releases
                   :all-articles all-articles})))
(when *command-line-args*
  (-main *command-line-args*))

(comment
  ;; pubDate 変換用
  (let [ts "2022/7/26 15:00:00"
        [d t] (str/split ts #" ")
        [year month date] (map parse-long (str/split d #"/"))
        [hour minute sec] (map parse-long (str/split t #":"))]
    (println
      (.format (ZonedDateTime/of year month date hour minute sec 0 (ZoneId/of "Asia/Tokyo"))
               DateTimeFormatter/RFC_1123_DATE_TIME))))
