(ns pingpong.model
  (:require [clojure.set :refer [difference intersection]]))

(defonce follow-games (atom {}))

(defonce last-changed-uid (atom nil))

(defonce taken-uid-nums (atom ()))


;; Helper function to pick number for new uid.
(defn smallest-new-num! [num-list]
  (let [len (count num-list)]
    (loop [try-num 1
           index 0]
      (if (and (< index len) 
               (>= try-num (nth num-list index)))
        (recur (inc try-num) (inc index))
        (do (reset! taken-uid-nums (sort (conj num-list try-num)))
            try-num)))))


;; Gives uid to every client.
(defn uid-to-client! [ring-req]
  (format "user-%d" (smallest-new-num! @taken-uid-nums)))


;; Add uid to game.
(defn uid-to-game! [client-uid]
  (let [games @follow-games
        uids (keys games)]
    ;; Try find opponent
    (loop [u-list uids]
      (if (empty? u-list)
        ;; No other players or all games are full.
        (swap! follow-games assoc client-uid {:game-on false
                                              :host? true
                                              :opp-uid nil
                                              :state nil
                                              :callback nil})
        (let [uid (first u-list)
              {:keys [opp-uid]} (get games uid)]
          (if opp-uid
            (recur (rest u-list))
            (do ;; Opponent found. Change also opponents state.
              (swap! follow-games assoc-in [uid :opp-uid] client-uid)
              (swap! follow-games assoc-in [uid :game-on] true)
              (swap! follow-games assoc client-uid {:game-on true
                                                    :host? false
                                                    :opp-uid uid
                                                    :state nil
                                                    :callback nil}))))))))


;; Remove uids that don't exist from follow-games and update taken-uid-nums.
(defn update-book-keeping! [connected-uids & uid]
  ;; If uid passed remove from follow-games
  (when uid 
    (swap! follow-games dissoc (first uid)))
  (let [games @follow-games
        games-set (set (keys games))
        uids-set (set connected-uids)
        correct-uids (intersection games-set uids-set)
        left-overs (difference games-set uids-set)]
    
    (prn "uids now" correct-uids)
    
    ;; Remove false uids.
    (doseq [false-uid left-overs]
      (swap! follow-games dissoc false-uid))
    ;; Update uid numbers.
    (reset! taken-uid-nums (map #(Integer/parseInt (str( last %))) 
                                correct-uids))))
