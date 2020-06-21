(ns pingpong.components.ui
  (:require [com.stuartsierra.component :as component]
            [pingpong.core :refer [run-game]]))

(defrecord UIComponent []
  component/Lifecycle
  (start [component]
    (run-game)
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
