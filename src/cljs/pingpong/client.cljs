(ns pingpong.client
  (:require [taoensso.sente :as sente :refer [cb-success?]]))


;;; Sente channels --->
(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" {:type :auto})]

  (def chsk       chsk)
  (def ch-chsk    ch-recv)  ;; ChannelSocket's receive channel
  (def chsk-send! send-fn)  ;; ChannelSocket's send API fn
  (def chsk-state state))   ;; Watchable, read-only atom


(def bat-height 100)
(def ball-start-speed 5)


;; Here we store server state.
(defonce server-state (atom {:ball [0 0]
                             :ball-dir [(dec (* 2 (rand-int 2))) 0]
                             :ball-speed ball-start-speed
                             :player-bat (- (/ bat-height 2))
                             :opponent-bat  (- (/ bat-height 2))
                             :player-bat-dir 0
                             :opponent-bat-dir 0
                             :player-score 0
                             :opponent-score 0
                             :game-on? false
                             :host? true
                             :state-used? false}))


;; Send state to server.
(defn send-state-to-server!
  [{:keys [ball ball-dir ball-speed player-bat player-bat-dir 
           player-score opponent-score]}]
  (chsk-send!
    [:pingpong/state [ball 
                      ball-dir 
                      ball-speed 
                      player-bat 
                      player-bat-dir
                      player-score 
                      opponent-score]]
    200 ;; Timeout
    (fn [reply]
      (when (cb-success? reply)
        (swap! server-state into (zipmap [:ball
                                          :ball-dir
                                          :ball-speed
                                          :player-bat
                                          :player-bat-dir
                                          :opponent-bat
                                          :opponent-bat-dir
                                          :player-score
                                          :opponent-score] reply))
        (swap! server-state assoc :state-used? false)))))


;;; Event handler --->
(defmulti event :id)

(defmethod event :default [{:keys [event]}]
  (prn "Default client" event))


;; This msg from server determines is game on and is client game host.
(defmethod event :chsk/recv [{:as ev-msg :keys [?data]}]
  (case (first ?data)
    :pingpong/game-on? (swap! server-state assoc :game-on? (second ?data))
    :pingpong/host-yes! (swap! server-state assoc :host? true)
    :pingpong/not-host! (swap! server-state assoc :host? false)
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
