(ns pingpong.common)

(def frame-rate 60)
(def ball [0 0])
(def ball-start-dir [(dec (* 2 (rand-int 2))) 0]) ;; Random start direction.
(def ball-start-speed 5)
(def bat-height 100)

(def starting-state {:ball ball
                     :ball-dir ball-start-dir
                     :ball-speed ball-start-speed
                     :player-bat (- (/ bat-height 2))
                     :opponent-bat  (- (/ bat-height 2))
                     :player-bat-dir 0
                     :opponent-bat-dir 0
                     :up-pressed false
                     :down-pressed false
                     :last-pressed nil
                     :game-on false})

(defn reverse-x [v]
  [(- (first v)) (second v)])

(defonce p1-start [ball
                   ball-start-dir
                   ball-start-speed
                   0])

(defonce p2-start [ball
                   (reverse-x ball-start-dir)
                   ball-start-speed
                   0])
