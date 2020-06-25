(ns pingpong.ping
  (:require [quil.core :as q :include-macros true]))

(defn check-bat-player [{:keys [ball ball-dir player-bat ball-speed]} 
                        {:keys [size bat-width bat-height ball-diameter]}]
  (let [ball-radius (/ ball-diameter 2)
        ball-edge (+ (first ball) ball-radius)
        bat-edge (- (/ (first size) 2) bat-width)]
    (and (< ball-edge bat-edge)
         (< bat-edge (+ ball-edge (* 2 ball-speed (first ball-dir))))
         (and (> (second ball) (- player-bat ball-radius)) 
              (< (second ball) (+ player-bat bat-height ball-radius))))))

(defn check-bat-opponent [{:keys [ball ball-dir opponent-bat ball-speed]} 
                          {:keys [size bat-width bat-height ball-diameter]}]
  (let [ball-radius (/ ball-diameter 2)
        ball-edge (- (first ball) ball-radius)
        bat-edge (- bat-width (/ (first size) 2))]
    (and (< bat-edge ball-edge)
         (< (+ ball-edge (* 2 ball-speed (first ball-dir))) bat-edge)
         ;; web-extra added to help non-host player!
         (and (> (second ball) (- opponent-bat ball-radius))
              (< (second ball) (+ opponent-bat bat-height ball-radius))))))

(defn check-roof-floor [{:keys [ball ball-dir ball-speed]} 
                        {:keys [size ball-diameter]}]
  (let [ball-roof-edge (- (second ball) (/ ball-diameter 2))
        ball-floor-edge (+ (second ball) (/ ball-diameter 2))
        roof-edge (- (/ (second size) 2))
        floor-edge (/ (second size) 2)]
    (or (and (> ball-roof-edge roof-edge)
             (< (+ ball-roof-edge (* ball-speed (second ball-dir))) roof-edge))
        (and (< ball-floor-edge floor-edge)
             (> (+ ball-floor-edge (* ball-speed (second ball-dir))) floor-edge)))))

(defn rotate [v angle]  ;; Counter-clockwise with positive angle.
  [(- (* (first v) (q/cos angle)) (* (second v) (q/sin angle)))
   (+ (* (first v) (q/sin angle)) (* (second v) (q/cos angle)))])

(defn reverse-x [v]
  [(- (first v)) (second v)])

(defn player-hit-bat [ball-dir bat-dir]
  (let [angle (- q/PI (q/acos (second ball-dir)))
        turned-dir (reverse-x ball-dir)]
    (case bat-dir
      0 turned-dir
      1 (rotate turned-dir (/ angle 2))
      -1 (rotate turned-dir (- (/ angle 2))))))

(defn opponent-hit-bat [ball-dir bat-dir]
  (let [angle (- q/PI (q/acos (second ball-dir)))
        turned-dir (reverse-x ball-dir)]
    (case bat-dir
      0 turned-dir
      -1 (rotate turned-dir (/ angle 2))
      1 (rotate turned-dir (- (/ angle 2))))))

(defn hit-rf [ball-dir]
  [(first ball-dir) (- (second ball-dir))])

(defn calc-bat-dir [{:keys [up-pressed? down-pressed? last-pressed]}]
  (if (and up-pressed? (= last-pressed :up))
    (- 1)
    (if (and down-pressed? (= last-pressed :down))
      1
      0)))

(defn calc-new-ball-dir 
  [{:as state :keys [player-bat-dir opponent-bat-dir ball-dir]} params]
  (cond
    (check-bat-player state params) 
      (player-hit-bat ball-dir player-bat-dir)
    
    (check-bat-opponent state params) 
      (opponent-hit-bat ball-dir opponent-bat-dir)
    
    (check-roof-floor state params) 
      (hit-rf ball-dir)
    
    :else ball-dir))

(defn round [v multiplier]
  (let [f #(/ (.round js/Math (* (+ % 0.0001) multiplier)) multiplier)]
    (if (number? v)
      (f v)
      (mapv f v))))

(defn check-reset [size ball ball-dir ball-speed ball-start-speed]
  (let [p-score? (< (first ball) (- (/ (first size) 2)))
        opp-score? (> (first ball) (/ (first size) 2))]
    (if p-score? 
      [1 0]
      (if opp-score?
        [0 1]
        [nil nil]))))
