(ns aps.minigraph.dev.app
  "Development app for testing minigraph components."
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [aps.minigraph.models :as m]
            [aps.minigraph.uix.canvas :refer [canvas]]))

;; Sample graph data
(defonce sample-graph
  (m/graph
   {:nodes [(m/node {:id "n1" :x 100 :y 100 :width 120 :height 80
                     :data {:label "Node 1"}})
            (m/node {:id "n2" :x 300 :y 150 :width 120 :height 80
                     :data {:label "Node 2"}})
            (m/node {:id "n3" :x 200 :y 300 :width 120 :height 80
                     :data {:label "Node 3"}})]
    :edges [(m/edge {:id "e1" :source "n1" :target "n2"
                     :data {:label "Edge 1"}})
            (m/edge {:id "e2" :source "n2" :target "n3"
                     :data {:label "Edge 2"}})]}))

(defui app []
  (let [[graph set-graph!] (uix/use-state sample-graph)]
    ($ :div
       {:style {:padding "20px"
                :font-family "system-ui, sans-serif"}}

       ($ :h1 "🕸️ Minigraph playground")

       ($ :p
          "Testing canvas component with "
          (count (:nodes graph)) " nodes and "
          (count (:edges graph)) " edges.")

       ($ canvas
          {:graph graph
           :width 800
           :height 600
           :on-viewport-change (fn [viewport]
                                 (js/console.log "Viewport changed:" viewport))
           :on-node-click (fn [node-id _event]
                            (js/console.log "Node clicked:" node-id))
           :on-node-drag-end (fn [node-id x y]
                               (js/console.log "Node dragged:" node-id "to" x y)
                               (set-graph!
                                (m/update-node graph node-id
                                               (fn [node]
                                                 (assoc node :x x :y y)))))
           :on-canvas-click (fn [x y _event]
                              (js/console.log "Canvas clicked:" x y))}))))

(defonce root
  (uix.dom/create-root (js/document.getElementById "app")))

(defn init []
  (js/console.log "Minigraph dev app initializing...")
  (uix.dom/render-root ($ app) root))

(defn reload []
  (js/console.log "Reloading...")
  (uix.dom/render-root ($ app) root))

(defn ^:export main []
  (init))
