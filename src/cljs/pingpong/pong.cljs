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
(def speed-inc 0.005)
(def bat-speed 6)
(def ball-error 5)
(def speed-error 1)
(def bat-error 5)

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
  [{:as p-state :keys [ball ball-dir ball-speed player-bat player-bat-dir]} 
   s-state]
  (let [s-speed (:ball-speed s-state)
        s-ball (mapv + (:ball s-state) (map #(* s-speed %) 
                                            (:ball-dir s-state)))
        s-bat (+ (:player-bat s-state) (* bat-speed (:player-bat-dir s-state)))
        ball-err (distance ball s-ball)
        speed-err (q/abs (- ball-speed s-speed))
        p-err (q/abs (- player-bat s-bat))]
    (cond-> p-state
      (> ball-err ball-error)
        (assoc :ball s-ball)
      (> speed-err speed-error) 
        (assoc :ball-speed s-speed)
      (> p-err bat-error) 
        (assoc :player-bat s-bat))))

(defn make-updates
  [{:as p-state :keys [ball ball-dir ball-speed]}
   {:as s-state :keys [player-bat-dir opponent-bat-dir game-on?
                       player-score opponent-score]}]
  (let [game-state (if game-on?
                     (check-and-fix-errors p-state s-state)
                     p-state)
        game-state (-> game-state
                    (update :player-bat + (* bat-speed player-bat-dir))
                    (update :opponent-bat + (* bat-speed opponent-bat-dir))
                    (assoc :player-bat-dir player-bat-dir)
                    (assoc :opponent-bat-dir opponent-bat-dir))
        new-ball (mapv + ball (map #(* ball-speed %) ball-dir))
        new-ball-dir (calc-new-ball-dir game-state params)
        new-ball-speed (+ ball-speed speed-inc)
        [p-score-inc
         opp-score-inc] (check-reset size new-ball new-ball-dir 
                                     new-ball-speed ball-start-speed)]
    (if p-score-inc
      (-> game-state
        (assoc :game-on? game-on?)
        (assoc :ball [0 0])
        (assoc :ball-dir [(dec (* 2 (rand-int 2))) 0])
        (assoc :ball-speed ball-start-speed)
        (update :player-score + p-score-inc)
        (update :opponent-score + opp-score-inc))
      (-> game-state
        (assoc :game-on? game-on?)
        (assoc :ball new-ball)
        (assoc :ball-dir new-ball-dir)
        (assoc :ball-speed new-ball-speed)))))

(defn update-state [p-state]
  (let [bat-dir (calc-bat-dir p-state)
        {:as s-state :keys [game-on?]} @server-state
        game-state (if game-on?
                    s-state
                    (assoc s-state :player-bat-dir bat-dir))]
    (send-state-to-server! (assoc p-state :player-bat-dir bat-dir))
    (make-updates p-state game-state)))

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
