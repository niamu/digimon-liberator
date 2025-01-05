(ns liberator.core
  (:require
   [clojure.data.json :as json]
   [clojure.data.xml :as xml]
   [clojure.data.zip.xml :as zip-xml]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.zip :as zip]
   [hickory.core :as hickory]
   [hickory.select :as select]
   [hiccup.page :as page]
   [liberator.util :as util])
  (:import
   [java.awt Graphics2D]
   [java.awt.image BufferedImage Raster]
   [java.net URI]
   [javax.imageio ImageIO]))

(def domain "https://digimoncard.com")

(defn param
  [uri]
  (-> uri
      (.getQuery)
      (string/split #"&")
      (as-> #__ s
        (reduce (fn [accl kv]
                  (apply assoc accl (string/split kv #"=")))
                {}
                s))
      (get "param")))

(defn parse-pages
  [{:issue/keys [param] :as issue}]
  (-> (util/http-get (str domain
                          "/digimon_liberator/api/diazepam_hai.php")
                     {:query-params {:mode 1
                                     :file "extend_info.json"
                                     :reqtype 0
                                     :param param}})
      (json/read-str :key-fn
                     (comp keyword
                           (fn [s]
                             (-> s
                                 (string/replace #"([a-z])([A-Z])"
                                                 "$1-$2")
                                 (string/lower-case)))))
      :pages
      (as-> #__ pages
        (->> pages
             (map :step-rect)
             (map-indexed (fn [idx {:keys [width height]}]
                            {:page/number idx
                             :page/width width
                             :page/height height}))))))

(defn scramble-dimensions
  [{:issue/keys [param] :as issue}]
  (let [scramble (-> (util/http-get (str domain
                                         "/digimon_liberator/api/diazepam_hai.php")
                                    {:query-params {:mode 7
                                                    :file "face.xml"
                                                    :reqtype 0
                                                    :param param}})
                     (xml/parse-str)
                     zip/xml-zip
                     (zip-xml/xml-> :Face :Scramble zip/node)
                     first)]
    (assoc issue
           :issue/scramble-dimensions
           {:scramble/width (-> scramble
                                zip/xml-zip
                                (zip-xml/xml-> :Width zip-xml/text)
                                first
                                parse-long)
            :scramble/height (-> scramble
                                 zip/xml-zip
                                 (zip-xml/xml-> :Height zip-xml/text)
                                 first
                                 parse-long)})))

(defn parse-image
  [{:issue/keys [param scramble-dimensions] :as issue}
   {:page/keys [number width height]
    {part-dimensions :scramble/dimensions
     scramble-values :scramble/values} :page/scramble :as page}]
  (let [existing-image (io/file (str "resources/episode/"
                                     (:issue/name issue)
                                     "/images/"
                                     number
                                     ".jpg"))]
    (if (.exists existing-image)
      (assoc page :page/image (ImageIO/read (io/input-stream existing-image)))
      (let [data (util/http-get (str domain
                                     "/digimon_liberator/api/diazepam_hai.php")
                                {:as :byte-array
                                 :query-params {:mode 1
                                                :file (format "%04d_0000.bin"
                                                              number)
                                                :reqtype 0
                                                :param param}})
            image (ImageIO/read (io/input-stream data))
            unscrambled-image (ImageIO/read (io/input-stream data))
            ^Graphics2D g (.createGraphics unscrambled-image)]
        (doseq [idx (range (count scramble-values))]
          (let [part (nth scramble-values idx)
                src-x-offset (* (mod part (:scramble/width scramble-dimensions))
                                (:dimension/width part-dimensions))
                src-y-offset (* (quot part (:scramble/height scramble-dimensions))
                                (:dimension/height part-dimensions))
                dest-x-offset (* (mod idx (:scramble/width scramble-dimensions))
                                 (:dimension/width part-dimensions))
                dest-y-offset (* (quot idx (:scramble/height scramble-dimensions))
                                 (:dimension/height part-dimensions))
                image-part (.getSubimage image
                                         src-x-offset
                                         src-y-offset
                                         (:dimension/width part-dimensions)
                                         (:dimension/height part-dimensions))]
            (.fill g
                   (java.awt.Rectangle. dest-x-offset
                                        dest-y-offset
                                        (:dimension/width part-dimensions)
                                        (:dimension/height part-dimensions)))
            (.drawImage g
                        image-part
                        nil
                        (int dest-x-offset)
                        (int dest-y-offset))))
        (assoc page :page/image (if scramble-values
                                  unscrambled-image
                                  image))))))

(defn parse-scramble
  [{:issue/keys [param] :as issue} {:page/keys [number]}]
  (let [xml-zip (-> (util/http-get (str domain "/digimon_liberator/api/diazepam_hai.php")
                                   {:query-params {:mode 1
                                                   :file (format "%04d.xml" number)
                                                   :reqtype 0
                                                   :param param}})
                    (xml/parse-str)
                    zip/xml-zip)]
    (when (not= (-> xml-zip
                    (zip-xml/xml-> :Page :Part :Kind)
                    ffirst
                    :attrs
                    :scramble) "0")
      (-> xml-zip
          (zip-xml/xml-> :Page :Scramble zip/node)
          first
          :content
          string/join
          (string/split #",")
          (as-> #__ numbers
            (mapv parse-long numbers))))))

(defn parse-comic
  [{:issue/keys [param scramble-dimensions] :as issue}]
  (let [page-data (parse-pages issue)
        total-pages (count page-data)
        pages (pmap (fn [{:page/keys [number width height] :as page}]
                      (let [scramble-values (parse-scramble issue page)]
                        (->> (assoc page
                                    :page/scramble
                                    {:scramble/dimensions
                                     {:dimension/width
                                      (when scramble-values
                                        (* (quot (/ width
                                                    (get scramble-dimensions
                                                         :scramble/width))
                                                 8)
                                           8))
                                      :dimension/height
                                      (when scramble-values
                                        (* (quot (/ height
                                                    (get scramble-dimensions
                                                         :scramble/height))
                                                 8)
                                           8))}
                                     :scramble/values scramble-values})
                             (parse-image issue))))
                    page-data)]
    (doseq [{:page/keys [number image]} pages]
      (let [f (io/file (str "resources/episode/"
                            (:issue/name issue)
                            "/images/"
                            number
                            ".jpg"))]
        (when-not (.exists f)
          (.mkdirs (io/file (.getParent f)))
          (ImageIO/write image "jpg" f))))
    (assoc issue :issue/pages pages)))

(defn issues
  []
  (->> (util/http-get (str domain "/digimon_liberator/en/comic/"))
       hickory/parse
       hickory/as-hickory
       (select/select (select/descendant
                       (select/id "chapters")
                       (select/and (select/tag "li")
                                   (select/class "chapterListBox"))
                       (select/and (select/tag "a")
                                   (select/attr "href" some?))))
       (map (fn [el]
              (let [issue-name (some-> (select/select
                                        (select/descendant
                                         (select/class "chapterListTitle")
                                         (select/tag "p"))
                                        el)
                                       first
                                       :content
                                       string/join
                                       string/trim)
                    thumbnail (some-> (select/select
                                       (select/descendant
                                        (select/class "chapterListThumbnail")
                                        (select/tag "img"))
                                       el)
                                      first
                                      (get-in [:attrs :src]))
                    uri (URI. (str domain (-> el :attrs :href)))]
                (-> {:issue/uri uri
                     :issue/param (param uri)
                     :issue/thumbnail (str domain thumbnail)
                     :issue/name issue-name}
                    scramble-dimensions))))))

(defn save-thumbnail!
  [{:issue/keys [thumbnail] :as issue}]
  (let [out-file (io/file (str "resources/episode/"
                               (:issue/name issue)
                               "/images/_00.jpg"))]
    (when-not (.exists out-file)
      (with-open [in (io/input-stream thumbnail)
                  out (io/output-stream out-file)]
        (io/copy in out))))
  issue)

(defn render-comic!
  [{:issue/keys [pages] :as issue}]
  (save-thumbnail! issue)
  (spit (io/file (str "resources/episode/"
                      (:issue/name issue)
                      "/index.html"))
        (page/html5 {:lang "en"}
          [:head
           [:meta {:charset "utf-8"}]
           [:title (format "Digimon Liberator - %s"
                           (:issue/name issue))]
           [:meta {:content "width=device-width,initial-scale=1"
                   :name "viewport"}]
           (page/include-css "../../comic.css")]
          [:body
           [:h1 "Digimon Liberator"]
           [:h2 (:issue/name issue)]
           (for [{:page/keys [number]} pages]
             [:image {:loading "lazy"
                      :src (format "./images/%d.jpg" number)}])]))
  issue)

(defn render-index!
  [issues]
  (spit (io/file (str "resources/index.html"))
        (page/html5 {:lang "en"}
          [:head
           [:meta {:charset "utf-8"}]
           [:title "Digimon Liberator"]
           [:meta {:content "width=device-width,initial-scale=1"
                   :name "viewport"}]
           (page/include-css "./index.css")]
          [:body
           [:h1 "Digimon Liberator"]
           [:ul
            (for [issue issues]
              [:li
               [:a {:href (format "./episode/%s/index.html"
                                  (:issue/name issue))}
                [:img {:loading "lazy"
                       :src (format "./episode/%s/images/_00.jpg"
                                    (:issue/name issue))}]
                [:span (:issue/name issue)]]])]]))
  issues)

(defn -main
  [& args]
  (let [all-issues (issues)]
    (do (doseq [issue all-issues]
          ((comp render-comic! parse-comic) issue))
        (render-index! all-issues))))
