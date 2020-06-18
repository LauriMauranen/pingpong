(ns pingpong.client2
  (:require [taoensso.sente :as sente :refer [cb-success?]]))


(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" {:type :auto})]

  (def chsk       chsk)
  (def ch-chsk    ch-recv)  ;; ChannelSocket's receive channel
  (def chsk-send! send-fn)  ;; ChannelSocket's send API fn
  (def chsk-state state))   ;; Watchable, read-only atom


;; This is where opponent state is stored.
(defonce opponent-state (atom {:host? nil
                               :game-on false
                               :ball [0 0]
                               :opponent-bat 0
                               :opponent-bat-dir 0}))


(defn reverse-x [v]
  [(- (first v)) (second v)])


(defn send-state-to-server 
  [{:keys [ball player-bat player-bat-dir]}]
  (chsk-send!
    [:pingpong/state [(reverse-x ball)
                      player-bat
                      player-bat-dir]]
    100 ;; timeout
    (fn [reply]
      (when (cb-success? reply)
        (let [{:keys [game-on]} @opponent-state]
          (if game-on
            (swap! opponent-state into reply)
            (reset! opponent-state (-> reply
                                       (assoc :ball [0 0])
                                       (assoc :ball-dir 
                                              [(dec (* 2 (rand-int 2))) 0])
                                       (assoc :game-on true)))))))))


(defmulti event :id)

(defmethod event :default [{:keys [event]}]
  (prn event))


;; When other player leaves make this player host and set game off.
(defmethod event :chsk/recv [{:keys [?data]}]
  (when (= (first ?data) :pingpong/game-off)
    (swap! opponent-state assoc :game-on false)
    (swap! opponent-state assoc :host? false)))


;;; Router --->
(def router_ (atom nil))

(defn stop-router!
  ;; Stop the message router by calling the previously saved stop function
  [] (when-let [stop-f @router_] (stop-f)))

(defn start-router! []
  ;; Stop router first, then start and save the result (which is a stop 
  ;; callback) in `router_ `.
  (stop-router!)
  (reset! router_ (sente/start-chsk-router! ch-chsk event)))

(start-router!)
