(ns pingpong.pong
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [pingpong.ping :refer [check-reset calc-bat-dir calc-new-ball-dir]]
            [pingpong.client :refer [server-state send-state-to-server!
                                     ball-start-speed bat-height]]))

(def background-color 0)
(def bat-color 255)
(def ball-color 255)
(def size [500 500])
(def ball-diameter 30)
(def bat-width 35)
(def speed-inc 0)
(def bat-speed 6)
(def ball-error 200)
(def bat-error 30)

(def params {:size size
             :bat-width bat-width
             :bat-height bat-height
             :ball-diameter ball-diameter})

(defn setup []
  (q/frame-rate 60)
  @server-state)

(defn key-pressed [state event]
  (case (:key event)
    :m (-> state
           (assoc :down-pressed? true)
           (assoc :last-pressed :down))
    :k (-> state 
           (assoc :up-pressed? true)
           (assoc :last-pressed :up))
    state))

(defn key-released [state event]
  (case (:key event) 
    :m (assoc state :down-pressed? false)
    :k (assoc state :up-pressed? false)
    state))

(defn distance [v1 v2]
  (q/dist (first v1) (first v2) (second v1) (second v2)))

(defn check-and-fix-errors 
  [{:as state :keys [ball ball-speed player-bat]} s-state]
  (let [s-ball (:ball s-state)
        s-bat (:player-bat s-state)
        s-speed (:ba1l-speed s-state)]
    (cond-> state
      (> (distance ball s-ball) ball-error)
        (assoc :ball s-ball)
      (> (q/abs (- player-bat s-bat)) bat-error)
        (assoc :player-bat s-bat))))

(defn update-state [state]
  (send-state-to-server! state)
  (let [{:as s-state 
         :keys [player-bat-dir opponent-bat opponent-bat-dir player-score 
                opponent-score game-on? host? state-used?]} @server-state
        
        game-state (if game-on? 
                    (if (not state-used?)
                      (do (swap! server-state assoc :state-used? true)
                        (-> (check-and-fix-errors state s-state)
                          (update :player-bat + (* bat-speed player-bat-dir))
                          (update :opponent-bat + (* bat-speed 
                                                     opponent-bat-dir))
                          (assoc :player-bat-dir player-bat-dir)
                          (assoc :opponent-bat-dir opponent-bat-dir)))
                      (-> state
                        (update :player-bat + (* bat-speed 
                                                 (:player-bat-dir state)))
                        (update :opponent-bat + (* bat-speed 
                                                   (:opponent-bat-dir state)))))
                    (-> state
                      (update :player-bat + 
                              (* bat-speed (:player-bat-dir state)))
                      (assoc :player-score 0)
                      (assoc :opponent-score 0)))
        
        new-ball (mapv + (:ball game-state) (map #(* (:ball-speed game-state) %)
                                                 (:ball-dir game-state)))
        new-ball-dir (calc-new-ball-dir game-state params)
        new-ball-speed (+ (:ball-speed game-state) speed-inc)

        [final-ball
         final-ball-dir
         final-ball-speed
         p-score-inc
         opp-score-inc] (check-reset size 
                                     new-ball 
                                     new-ball-dir 
                                     new-ball-speed
                                     ball-start-speed)
        
        [p-score opp-score] (if host?
                              [(+ (:player-score game-state) p-score-inc)
                               (+ (:opponent-score game-state) opp-score-inc)]
                              [player-score opponent-score])]
    (-> game-state
      (assoc :game-on? game-on?)
      (assoc :ball final-ball)
      (assoc :ball-dir final-ball-dir)
      (assoc :ball-speed final-ball-speed)
      (assoc :player-bat-dir (calc-bat-dir state))
      (assoc :player-score p-score)
      (assoc :opponent-score opp-score))))

(defn draw-keys []
  (let [bottom (/ (q/height) 2)
        k-height (* bottom 0.7)
        m-height (* bottom 0.85)]
  (q/text-size 30)
  (q/text "K" 0 k-height)
  (q/text "M" -2 m-height)))

(defn draw-scores [{:keys [player-score opponent-score]}]
  (let [p-width (- (/ (q/width) 2) 50)
        opp-width (- 50 (/ (q/width) 2))
        p-opp-height (- 50 (/ (q/height) 2))]
  (q/text-size 25)
  (q/text-num player-score p-width p-opp-height)
  (q/text-num opponent-score opp-width p-opp-height)))

(defn draw-bats [{:keys [player-bat opponent-bat host?]}]
  (q/rect (- (/ (q/width) 2)) opponent-bat bat-width bat-height)
  (q/rect (- (/ (q/width) 2) bat-width) player-bat bat-width bat-height))

(defn debug [state]
  (q/text-size 25)
  (q/text-num (:ball state) 0 50))

(defn draw-state [{:as state :keys [ball game-on?]}]
  (q/background background-color)
  (q/fill 255)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-keys)
;;  (debug state)
  ;; Draw ball only when game is on!
  (when game-on?
    (draw-scores state)
    (q/ellipse (first ball) (second ball) ball-diameter 
               ball-diameter))
  (draw-bats state))

(defn run-sketch []
  (q/defsketch pingpong
    :title "Play pong!"
    :size size
    :setup setup
    :key-pressed key-pressed
    :key-released key-released
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))
