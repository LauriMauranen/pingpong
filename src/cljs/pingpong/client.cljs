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
  [{:keys [ball ball-dir player-bat player-bat-dir 
           player-score opponent-score]}]
  (let [host? (:host? @server-state)
        msg (if host?
              [(reverse-x ball) player-bat opponent-score player-score]
              [player-bat player-bat-dir])]
    (chsk-send! 
      [:pingpong/state msg]
      200 ;; timeout ms
      (fn [reply]
        (if (sente/cb-success? reply)
          (if host?
            (when (= (count reply) 2)
              (swap! server-state into {:game-on? true
                                        :opponent-bat (first reply)
                                        :opponent-bat-dir (second reply)}))
            (when (= (count reply) 4)
              (swap! server-state into {:game-on? true
                                        :ball (first reply)
                                        :opponent-bat (second reply)
                                        :player-score (nth reply 2) 
                                        :opponent-score (nth reply 3)}))))))))


;; Event handler --->
(defmulti event :id)

(defmethod event :default [{:keys [event]}]
  (prn "Default client" event))


;; This msg from server determines is client host.
(defmethod event :chsk/recv [{:as ev-msg :keys [?data]}]
  (case (first ?data)
    :pingpong/host? (do (swap! server-state assoc :host? (second ?data))
                        (swap! server-state assoc :game-on? false))
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
