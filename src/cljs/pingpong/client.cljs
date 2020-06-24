(ns pingpong.client
  (:require [taoensso.sente :as sente :refer [cb-success?]]))

;;; Sente channels --->
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


;; Send state to server.
(defn send-state-to-server!
  [{:keys [ball player-bat opponent-bat player-bat-dir 
           player-score opponent-score]}]
  (let [host? (:host? @server-state)
        msg (if host?
              ;; Host sends lot's of things.
              [(reverse-x ball) opponent-bat player-bat
               opponent-score player-score]
              ;; Non-host sends only bat-dir.
              player-bat-dir)]
    (chsk-send! 
      [:pingpong/state msg]

      ;; Timeout, important!!!
      250

      (fn [reply]
        (when (cb-success? reply)
          (if host?
            (when (number? reply)
              (swap! server-state into {:game-on? true
                                        :opponent-bat-dir reply}))
            (when (= (count reply) 5)
              (swap! server-state into {:game-on? true
                                        :ball (first reply)
                                        :player-bat (second reply)
                                        :opponent-bat (nth reply 2)
                                        :player-score (nth reply 3) 
                                        :opponent-score (nth reply 4)}))))))))


;;; Event handler --->
(defmulti event :id)

(defmethod event :default [{:keys [event]}]
  (prn "Default client" event))


;; This msg from server determines is client host in game.
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
