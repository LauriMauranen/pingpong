(ns pingpong.core
  (:require [reagent.core :as reagent :refer [atom]]
            [pingpong.pong :refer [run-sketch]]))

(enable-console-print!)

(defn run-game ^:export []
  (run-sketch))

(defn render []
  (reagent/render [(fn [] [:p ""])] (js/document.getElementById "app")))
