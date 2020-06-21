(ns pingpong.model
  (:require [clojure.set :refer [difference intersection]]))

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
          (chsk-send! client-uid [:pingpong/host? true]))
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
              ;; Tell client she's not host.
              (chsk-send! client-uid [:pingpong/host? false]))))))))
