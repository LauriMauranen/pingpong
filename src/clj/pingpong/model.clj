(ns pingpong.model)

(defonce follow-games (atom {}))

(defonce last-changed-uid (atom nil))

(defonce taken-uid-nums (atom []))


;; Helper function to determine new clients uid.
(defn smallest-new-uid-num []
  (let [taken-nums @taken-uid-nums
        len (count taken-nums)]
    (loop [try-num 1
           index 0]
      (if (and (< index len) 
               (>= try-num (nth taken-nums index)))
        (recur (inc try-num) (inc index))
        (do (reset! taken-uid-nums (sort (conj taken-nums try-num)))
            try-num)))))


;; Adds new client to game list where msgs are controlled.
(defn player-to-game-list [{:keys [params]}]
  (let [client-id (:client-id params)
        games @follow-games
        uids (keys games)]
    (loop [u-list uids]
      (if (empty? u-list)
        ;; All games are full or no games at all. Make new game.
        (let [new-uid (format "game-%d" (smallest-new-uid-num))]
          (swap! follow-games assoc new-uid {:game-on false
                                             :player-1 client-id
                                             :player-2 nil
                                             :p1-state nil
                                             :p2-state nil
                                             :p1-callback nil
                                             :p2-callback nil
                                             :p1-timer nil
                                             :p2-timer nil})
          new-uid)
        (let [uid (first u-list)
              {:keys [game-on player-1]} (get games uid)]
          (if game-on
            (recur (rest u-list))
            (do ;; Add client to existing non-full game and start game.
              (swap! follow-games assoc-in [uid :player-2] client-id)
              (swap! follow-games assoc-in [uid :game-on] true)
              uid)))))))
