(ns liberator.util
  (:require
   [clj-http.client :as http]
   [clj-http.conn-mgr :as conn-mgr]))

(def ^:private connection-manager
  (conn-mgr/make-reusable-conn-manager {:timeout 20
                                        :default-per-route 20}))

(defn http-get
  [url & [params]]
  (-> (http/get url (merge params
                           {:socket-timeout 20000
                            :connection-timeout 20000
                            :connection-manager connection-manager
                            :cache true
                            :retry-handler
                            (fn [ex try-count _]
                              (if (> try-count 2)
                                (do (prn
                                     (format "Failed after %d attempts"
                                             try-count))
                                    false)
                                true))}))
      :body))
