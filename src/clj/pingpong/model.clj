(ns pingpong.model
  (:require [clojure.set :refer [difference]]))

(defonce follow-games (atom {}))

(defonce last-changed-uid (atom nil))


;; Helper function to pick number for new uid.
(defn smallest-new-num! []
  (let [games @follow-games
        ;; These numbers are already in use.
        nums (sort (map #(Integer/parseInt (str( last %))) (keys games)))
        len (count nums)]
    (loop [try-num 1
           index 0]
      (if (and (< index len) 
               (>= try-num (nth nums index)))
        (recur (inc try-num) (inc index))
        try-num))))


(defn make-p1-state
  [[ball ball-dir ball-speed p1-bat p1-bat-dir p1-score p2-score]
   [_ _ _ p2-bat p2-bat-dir _ _]]
  [ball
   ball-dir
   ball-speed
   p1-bat
   p1-bat-dir
   p2-bat
   p2-bat-dir
   p1-score
   p2-score])


;; Reverse x-axis because both players see's themselves on right.
(defn reverse-x [v]
  [(- (first v)) (second v)])


(defn make-p2-state
  [[ball ball-dir ball-speed p1-bat p1-bat-dir p1-score p2-score]
   [_ _ _ p2-bat p2-bat-dir _ _]]
  [(reverse-x ball)
   (reverse-x ball-dir)
   ball-speed
   p2-bat
   p2-bat-dir
   p1-bat
   p1-bat-dir
   p2-score
   p1-score])


;; Gives uid to every client.
(defn uid-to-client! [ring-req]
  (format "user-%d" (smallest-new-num!)))


;; Add uid to game.
(defn uid-to-game! [client-uid chsk-send!]
  (let [games @follow-games
        uids (keys games)]
    ;; Try find opponent
    (loop [u-list uids]
      (if (empty? u-list)
        (do ;; No other players or all games are full.
          (swap! follow-games assoc client-uid {:host? true
                                                :opp-uid nil
                                                :state nil
                                                :callback nil})
          ;; Tell client she is host.
          (chsk-send! client-uid [:pingpong/game-on? false]))
        (let [uid (first u-list)
              {:keys [opp-uid]} (get games uid)]
          (if opp-uid
            (recur (rest u-list))
            (do ;; Opponent found. Change also opponents state.
              (swap! follow-games assoc-in [uid :opp-uid] client-uid)
              (swap! follow-games assoc client-uid {:host? false
                                                    :opp-uid uid
                                                    :state nil
                                                    :callback nil})
              ;; Game on!
              (chsk-send! uid [:pingpong/game-on? true])
              (chsk-send! client-uid [:pingpong/game-on? true]))))))))
