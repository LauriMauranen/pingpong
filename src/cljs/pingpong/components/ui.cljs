(ns pingpong.components.ui
  (:require [com.stuartsierra.component :as component]
            [pingpong.core :refer [render run-game]]))

(defrecord UIComponent []
  component/Lifecycle
  (start [component]
    (run-game) ;; (render)
    component)
  (stop [component]
    component))

(defn new-ui-component []
  (map->UIComponent {}))
