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
  (let [[graph set-graph!] (uix/use-state sample-graph)
        [node-counter set-node-counter!] (uix/use-state 4)

        add-node!
        (fn []
          (let [new-id (str "n" node-counter)
                ;; Place new nodes in a grid-like pattern
                col (mod (dec node-counter) 4)
                row (quot (dec node-counter) 4)
                x (+ 100 (* col 200))
                y (+ 100 (* row 150))
                new-node (m/node {:id new-id
                                  :x x
                                  :y y
                                  :width 120
                                  :height 80
                                  :data {:label (str "Node " node-counter)}})]
            (when-let [new-graph (m/add-node graph new-node)]
              (set-graph! new-graph)
              (set-node-counter! (inc node-counter)))))]

    ($ :div
       {:style {:padding "20px"
                :font-family "system-ui, sans-serif"}}

       ($ :h1 "🕸️ Minigraph playground")

       ($ :div
          {:style {:display "flex"
                   :gap "20px"
                   :align-items "center"
                   :margin-bottom "20px"}}

          ($ :p
             {:style {:margin 0}}
             "Testing canvas component with "
             (count (:nodes graph)) " nodes and "
             (count (:edges graph)) " edges.")

          ($ :button
             {:style {:padding "8px 16px"
                      :background "#2196f3"
                      :color "white"
                      :border "none"
                      :border-radius "4px"
                      :cursor "pointer"
                      :font-size "14px"}
              :on-click add-node!}
             "+ Add Node"))

       ($ :p
          {:style {:color "#666" :font-size "14px"}}
          "💡 Tip: Drag from the center to move nodes. Drag from the border to create edges.")

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
           :on-edge-create (fn [edge-data]
                             (js/console.log "Edge created:" edge-data)
                             (when-let [new-graph (m/add-edge graph edge-data)]
                               (set-graph! new-graph)))
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
