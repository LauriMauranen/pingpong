(ns pingpong.pong
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [pingpong.ping2 :refer [check-reset calc-bat-dir calc-new-ball-dir]]
            [pingpong.client :refer [server-state send-state-to-server!
                                     ball-start-speed bat-height]]))

(def background-color 0)
(def bat-color 255)
(def ball-color 255)
(def size [500 500])
(def ball-diameter 30)
(def bat-width 35)
(def speed-inc 0.01)
(def bat-speed 10)

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

(defn make-updates
  [{:as s-state :keys [ball ball-dir ball-speed player-bat player-bat-dir
                       opponent-bat-dir game-on?]}]
  (let [game-state (-> s-state
                    (update :player-bat + (* bat-speed player-bat-dir))
                    (update :opponent-bat + (* bat-speed opponent-bat-dir)))
        new-ball (mapv + ball (map #(* ball-speed %) ball-dir))
        new-ball-dir (calc-new-ball-dir game-state params)
        new-ball-speed (+ ball-speed speed-inc)
        [final-ball 
         final-ball-dir
         final-ball-speed
         p-score-inc
         opp-score-inc] (check-reset size new-ball new-ball-dir 
                                     new-ball-speed ball-start-speed)]
    (-> game-state
      (assoc :ball final-ball)
      (assoc :ball-dir final-ball-dir)
      (assoc :ball-speed final-ball-speed)
      (update :player-score + p-score-inc)
      (update :opponent-score + opp-score-inc))))

(defn update-state [state]
  (let [bat-dir (calc-bat-dir state)
        {:as s-state :keys [game-on?]} @server-state
        game-state (if game-on?
                    s-state
                    (update state :player-bat + (* bat-speed bat-dir)))]
    (send-state-to-server! (assoc state :player-bat-dir bat-dir))
    (make-updates (into state game-state))))

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

(defn draw-state [{:as state :keys [ball game-on?]}]
  (q/background background-color)
  (q/fill 255)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-keys)
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
