(ns build
  (:require
   [babashka.curl :as curl]
   [cheshire.core :as json]
   [clojure.string :as str]))

(def base-query "
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
        releases(last:1) {
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
  [all-releases]
  (->> all-releases
       (sort-by published-at)
       (reverse)
       (take 5)))

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

(defn -main
  [[oauth-token]]
  (let [template (slurp "README.tmpl")
        all-releases (fetch-all-releases oauth-token)
        releases (recent-releases all-releases)
        links (->> (map release->adoc-link releases)
                   (map #(str "- " %))
                   (str/join "\n"))
        readme (-> template
                   (str/replace "<<links>>" links))]
    (spit "README.adoc" readme)))

(when *command-line-args*
  (-main *command-line-args*))
