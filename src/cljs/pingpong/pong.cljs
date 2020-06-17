(ns pingpong.pong
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [pingpong.ping :refer [check-reset-and-round calc-bat-dir 
                                   calc-new-ball-dir]]
            [pingpong.common :refer [frame-rate starting-state reverse-x]]
            [pingpong.client2 :refer [opponent-state send-state-to-server]]))

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

(def params {:size size
             :bat-width bat-width
             :bat-height bat-height
             :ball-diameter ball-diameter})

(defn setup []
  (q/frame-rate frame-rate)
  starting-state)

(defn key-pressed [state event]
  (case (:key event)
    :m (-> state
           (assoc-in [:down-pressed] true)
           (assoc-in [:last-pressed] :down))
    :k (-> state 
           (assoc-in [:up-pressed] true)
           (assoc-in [:last-pressed] :up))
    state))

(defn key-released [state event]
  (case (:key event) 
    :m (assoc-in state [:down-pressed] false)
    :k (assoc-in state [:up-pressed] false)
    state))

(defn make-updates 
  [{:as player-state :keys [ball ball-speed ball-dir]}
   {:keys [opponent-bat-dir game-on]}]
  (let [game-state (-> player-state 
                       (assoc :player-bat-dir (calc-bat-dir player-state))
                       (assoc :opponent-bat-dir opponent-bat-dir))
        new-ball (mapv + ball (map #(* ball-speed %) ball-dir))
        new-ball-dir (calc-new-ball-dir game-state params)
        new-ball-speed (+ ball-speed speed-inc)
        [final-ball 
         final-ball-dir
         final-ball-speed] (check-reset-and-round size 
                                                  new-ball 
                                                  new-ball-dir 
                                                  new-ball-speed 
                                                  ball-start-speed)]
    (-> game-state
      (assoc :game-on game-on)
      (assoc :ball final-ball)
      (assoc :ball-dir final-ball-dir)
      (assoc :ball-speed final-ball-speed)
      (update :player-bat + (* bat-speed (:player-bat-dir game-state)))
      (update :opponent-bat + (* bat-speed opponent-bat-dir)))))

(defn update-state [player-state]
  (let [bat-dir (calc-bat-dir player-state)
        {:as opp-state :keys [host?]} @opponent-state]
;;    (when host?
;;    (prn "opp-state" opp-state)
;;    (prn "player-state" player-state))
    (send-state-to-server (-> player-state 
                              (assoc :player-bat-dir bat-dir)))
    (if host?
      (-> player-state 
          (into (dissoc opp-state :host?))
          (update :player-bat + (* bat-speed bat-dir)))
      (make-updates player-state opp-state))))

(defn draw-keys []
  (let [bottom (/ (q/height) 2)
        k-height (* bottom 0.7)
        m-height (* bottom 0.85)]
  (q/text-size 30)
  (q/text "K" 0 k-height)
  (q/text "M" -2 m-height)))

(defn draw-bats [{:keys [player-bat opponent-bat]}]
  (q/rect (- (/ (q/width) 2)) opponent-bat bat-width bat-height)
  (q/rect (- (/ (q/width) 2) bat-width) player-bat bat-width bat-height))

(defn debugf [{:as state :keys [ball ball-dir ball-speed]}]
;;    (prn state)
    (q/fill 255)
    (q/text-size 20)
    (q/text "" 0 100))

(defn draw-state [{:as state :keys [ball game-on]}]
  (q/background background-color)
  (q/fill 255)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-keys)
  ;; Draw ball only when game is on!
  (when game-on
    (q/ellipse (first ball) (second ball) ball-diameter 
               ball-diameter))
  (draw-bats state)
  (debugf state)
  )

(defn run-sketch []
  (q/defsketch pingpong
    :title "Play pong"
    :size size
    :setup setup
    :key-pressed key-pressed
    :key-released key-released
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))
