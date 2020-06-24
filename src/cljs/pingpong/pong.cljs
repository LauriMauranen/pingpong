(ns pingpong.pong
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [pingpong.ping2 :refer [check-reset calc-bat-dir calc-new-ball-dir]]
            [pingpong.client :refer [server-state send-state-to-server!]]))

(def background-color 0)
(def bat-color 255)
(def ball-color 255)
(def size [500 500])
(def ball-diameter 30)
(def bat-width 35)
(def bat-height 100)
(def ball-start-speed 5)
(def speed-inc 0.005)
(def bat-speed 6)

(def bat-delay 7)
(defonce delay-atom (atom '()))

(def params {:size size
             :bat-width bat-width
             :bat-height bat-height
             :ball-diameter ball-diameter})

(defn setup []
  (q/frame-rate 60)
  {:ball [0 0]
   :ball-dir [(dec (* 2 (rand-int 2))) 0] ;; Random direction.
   :ball-speed ball-start-speed
   :player-bat (- (/ bat-height 2))
   :opponent-bat  (- (/ bat-height 2))
   :player-bat-dir 0
   :opponent-bat-dir 0
   :player-score 0
   :opponent-score 0
   :up-pressed? false
   :down-pressed? false
   :last-pressed nil
   :game-on? false
   :host? true})

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

;; Delay host bat movement because non-host player has internet lag.
(defn delay-p-bat! [bat-dir]
  (swap! delay-atom conj bat-dir)
  (let [prev-moves @delay-atom]
    (if (= (count prev-moves) bat-delay)
      ;; Remove and return oldest move.
      (do (swap! delay-atom butlast)
          (last prev-moves))
      ;; Or return 0 so bat doesn't move.
      0)))

(defn make-updates 
  [{:as player-state :keys [ball ball-speed ball-dir player-bat-dir]}
   {:keys [opponent-bat-dir game-on?]}]
  (let [delayed-bat-dir (delay-p-bat! player-bat-dir)
        game-state (-> player-state
                       (assoc :player-bat-dir delayed-bat-dir)
                       (assoc :opponent-bat-dir opponent-bat-dir)
                       (update :player-bat + (* bat-speed delayed-bat-dir))
                       (update :opponent-bat + (* bat-speed opponent-bat-dir)))

        ;; When game is off set opp-bat to middle, set scores to zero and
        ;; don't speed up ball.
        game-state (if game-on?
                    game-state
                    (-> game-state 
                        (assoc :opponent-bat (- (/ bat-height 2)))
                        (assoc :player-score 0)
                        (assoc :opponent-score 0)
                        (assoc :ball-speed ball-start-speed)))

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
      (assoc :game-on? game-on?)
      (assoc :ball final-ball)
      (assoc :ball-dir final-ball-dir)
      (assoc :ball-speed final-ball-speed)
      (update :player-score + p-score-inc)
      (update :opponent-score + opp-score-inc))))

(defn update-state [player-state]
  (let [bat-dir (calc-bat-dir player-state)
        new-p-state (assoc player-state :player-bat-dir bat-dir)
        {:as s-state :keys [host?]} @server-state]
    (send-state-to-server! new-p-state)
    (if host?
      ;; Host evaluates game state.
      (make-updates new-p-state s-state)
      ;; Non-host just draws state from server.
      (into player-state s-state))))

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
