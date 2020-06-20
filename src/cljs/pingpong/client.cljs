(ns pingpong.client
  (:require [taoensso.sente :as sente]))

;; Sente channels --->
(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" {:type :auto})]

  (def chsk       chsk)
  (def ch-chsk    ch-recv)  ;; ChannelSocket's receive channel
  (def chsk-send! send-fn)  ;; ChannelSocket's send API fn
  (def chsk-state state))   ;; Watchable, read-only atom


;; Here we store server state.
(defonce server-state (atom {:host? true
                             :game-on? false
                             :opponent-bat 0
                             :opponent-bat-dir 0}))


;; Reverse x-axis because both players see's themselves on right.
(defn reverse-x [v]
  [(- (first v)) (second v)])


;; Send state to server. What comes back depends on is client host.
(defn send-state-to-server!
  [{:keys [ball player-bat player-bat-dir player-score opponent-score]}]
  (chsk-send! 
    [:pingpong/state [(reverse-x ball)
                      player-bat
                      player-bat-dir
                      player-score
                      opponent-score]]
    100 ;; timeout
    (fn [reply]
      (when (sente/cb-success? reply)
        ;; If we get callback then game is on.
        (swap! server-state conj (assoc reply :game-on? true))))))


;; Event handler --->
(defmulti event :id)


(defmethod event :default [{:keys [event]}]
  (prn "Default client" event))


;; When other player leaves set game off and other player host.
(defmethod event :chsk/recv [{:as ev-msg :keys [?data]}]
  (case (first ?data)
    :pingpong/game-off (do (swap! server-state assoc :host? true)
                           (swap! server-state assoc :game-on? false)
;;                           (swap! server-state assoc :opponent-bat 0)
                           )
    :chsk/ws-ping nil
    (prn "Receive" ev-msg)))


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
