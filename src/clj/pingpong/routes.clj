(ns pingpong.routes
  (:require [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [compojure.route :refer [resources]]
            [ring.util.response :refer [response]]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [pingpong.model :as model :refer [follow-games last-changed-uid]]))


;;; Sente channels --->
(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) 
                                  {:csrf-token-fn nil
                                   :user-id-fn model/uid-to-client!})]

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


;; Send state to players. p1/p2 -state is [ball player-bat player-bat-dir].
(add-watch last-changed-uid 
           nil
           (fn [_ _ _ p1-uid]
            (let [games @follow-games
                  p1 (get games p1-uid)
                  p1-state (:state p1)
                  p2-uid (:opp-uid p1)
                  p1-host? (:host? p1)
                  p1-callback (:callback p1)]
;;              (prn "Watch" p1)
              (when p2-uid
                (let [p2 (get games p2-uid)
                      p2-state (:state p2)
                      p2-callback (:callback p2)]
                  ;; Server waits both players before sending new states.
                  (when (and p1-state p2-state)
                    (if p1-host?
                      ;; p1 is host. To host we send opponent-bat-dir,
                      ;; to other player [ball opp-bat].
                      (do (p1-callback (last p2-state))
                          (p2-callback [(first p1-state) (second p1-state)]))
                      ;; p2 is host.
                      (do (p2-callback (last p1-state))
                          (p1-callback [(first p2-state) (second p2-state)])))
                    ;; Reset states.
                    (swap! follow-games assoc-in [p1-uid :state] nil)
                    (swap! follow-games assoc-in [p2-uid :state] nil)))))))


;;; Events --->
(defmulti event :id)

(defmethod event :default [{:keys [event]}]
  (prn "Default" event))

(defmethod event :chsk/ws-ping [{:keys [event]}]
  (prn "Server" event))


;; Put new client to game.
(defmethod event :chsk/uidport-open [{:keys [uid]}]
  (prn "Client added to game" uid)
  (model/uid-to-game! uid chsk-send!)
  (prn @follow-games))


;; Remove offline client from game.
(defmethod event :chsk/uidport-close [{:keys [uid]}]
  (prn "Client removed from game" uid)
  (let [{:keys [opp-uid]} (get @follow-games uid)]
    ;; First make changes to follow-games and taken-uid-nums. 
    (model/update-book-keeping! (:any @connected-uids) uid)
    (when opp-uid
      ;; If opponent exists tell her game is off and move to her another game.
;;      (chsk-send! opp-uid [:pingpong/game-off nil])
      (model/uid-to-game! opp-uid chsk-send!))))


;; States from players.
(defmethod event :pingpong/state [{:keys [uid ?data ?reply-fn]}]
  (swap! follow-games assoc-in [uid :state] ?data)
  (swap! follow-games assoc-in [uid :callback] ?reply-fn)
  (reset! last-changed-uid uid))


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
