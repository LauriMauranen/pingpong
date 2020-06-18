(ns pingpong.routes
  (:require [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [compojure.route :refer [resources]]
            [ring.util.response :refer [response]]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [pingpong.model :refer [player-to-game-list follow-games
                                    last-changed-uid taken-uid-nums]]))


;;; Sente channels --->
(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) 
                                  {:csrf-token-fn nil
                                   :user-id-fn player-to-game-list})]

  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv);; ChannelSocket's receive channel
  (def chsk-send!                    send-fn);; ChannelSocket's send API fn
  (def connected-uids                connected-uids)); Watchable, read-only atom


(defn home-routes [endpoint]
  (routes
   (GET "/" _
     (-> "public/index.html"
         io/resource
         io/input-stream
         response
         (assoc :headers {"Content-Type" "text/html; charset=utf-8"})))
   (GET  "/chsk" req (ring-ajax-get-or-ws-handshake req))
   (POST "/chsk" req (ring-ajax-post                req))
   (resources "/")))


;; Player-1 is host so she sends ball and bat and player-2 sends only bat-dir.
;; p-state is [ball player-bat player-bat-dir].
(add-watch last-changed-uid 
           nil
           (fn [_ _ _ uid]
             (let [{:keys [p1-state p2-state p1-callback 
                           p2-callback]} (get @follow-games uid)]
              ;; Server waits both players before sending new states.
              (when (and p1-state p2-state)
                (when p1-callback
                  (p1-callback {:host? false
                                :opponent-bat-dir (last p2-state)
                                }))
                (when p2-callback
                  (p2-callback {:host? true
                                :ball (first p1-state)
                                :opponent-bat (second p1-state)
                                }))
                  (swap! follow-games assoc-in [uid :p1-state] nil)
                  (swap! follow-games assoc-in [uid :p2-state] nil)))))


(defn remove-game [uid]
  (let [uid-num (Integer/parseInt (str (last uid)))]
    (swap! follow-games dissoc uid)
    (swap! taken-uid-nums (partial remove #(= uid-num %)))))


;; Remove client from game. If player-2 is online change her to player-1.
(defn remove-client [uid player-num]
  ;; Tell other player game is off.
  (chsk-send! uid [:pingpong/game-off nil])
  (let [{:keys [player-1 player-2]} (get @follow-games uid)]
    (prn "Client removed" uid player-num)
    (cond
      (and (= player-num 1) player-2)
        (do ;; Change player-2 to player-1.
          (swap! follow-games assoc-in [uid :player-1] player-2)
          (swap! follow-games assoc-in [uid :p1-callback] nil)
          (swap! follow-games assoc-in [uid :p1-state] nil)
          (swap! follow-games assoc-in [uid :p1-timer] nil)
          (swap! follow-games assoc-in [uid :player-2] nil)
          (swap! follow-games assoc-in [uid :p2-callback] nil)
          (swap! follow-games assoc-in [uid :p2-state] nil)
          (swap! follow-games assoc-in [uid :p2-timer] nil)
          (swap! follow-games assoc-in [uid :game-on] false))
      (and (= player-num 2) player-1)
        (do ;; Remove player-2 from game.
          (swap! follow-games assoc-in [uid :player-2] nil)
          (swap! follow-games assoc-in [uid :p2-callback] nil)
          (swap! follow-games assoc-in [uid :p2-state] nil)
          (swap! follow-games assoc-in [uid :p2-timer] nil)
          (swap! follow-games assoc-in [uid :game-on] false)))))


;;; Events --->
(defmulti event :id)

(defmethod event :default [{:keys [event]}]
  (prn "Default" event))

(defmethod event :chsk/ws-ping [msg]
  nil)


;; States from players.
(defmethod event :pingpong/state [{:keys [uid ?data client-id ?reply-fn]}]
  (let [{:keys [game-on player-1 player-2 p1-state 
                p2-state p1-timer p2-timer]} (get @follow-games uid)
        player-1? (= player-1 client-id)
        player-2? (= player-2 client-id)]
    (when player-1?
      ;; Check player-2 timer. If realized remove client from game.
      (when p2-timer
        (when (realized? p2-timer)
          (remove-client uid 2)))
      (let [client-future (future (Thread/sleep 2000))]
        (swap! follow-games assoc-in [uid :p1-timer] client-future)
        (swap! follow-games assoc-in [uid :p1-state] ?data)
        (swap! follow-games assoc-in [uid :p1-callback] ?reply-fn)
        (reset! last-changed-uid uid)))
    (when player-2?
      ;; If player-1 timer clicks change player-2 to player-1.
      (when p1-timer
        (when (realized? p1-timer)
          (remove-client uid 1)))
        (let [client-future (future (Thread/sleep 2000))]
          (swap! follow-games assoc-in [uid :p2-timer] client-future)
          (swap! follow-games assoc-in [uid :p2-state] ?data)
          (swap! follow-games assoc-in [uid :p2-callback] ?reply-fn)
          (reset! last-changed-uid uid)))))


;; If game has no players delete from follow-games and free uid-num 
;; for future games.
(defmethod event :chsk/uidport-close [{:keys [uid]}]
  (prn "Game removed" uid)
  (remove-game uid))


;;; Router --->
(defonce router_ (atom nil))

;; Stop router if we aware of any router stopper callback function.
(defn stop-router! []
   (when-let [stop-f @router_] (stop-f)))

;; Stop and start router while storing the router stop-function in 
;; router_ atom.
(defn start-router! []
  (stop-router!)
  (reset! router_ (sente/start-chsk-router! ch-chsk event)))

(start-router!)
