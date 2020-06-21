(ns pingpong.core
  (:require [pingpong.pong :refer [run-sketch]]))

(enable-console-print!)

(defn run-game ^:export []
  (run-sketch))
