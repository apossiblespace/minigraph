(ns aps.minigraph.uix.edge
  "Edge component - Renders connections between nodes."
  (:require [uix.core :as uix :refer [defui $]]
            [aps.minigraph.geometry :as geo]))

(defui edge
  "Edge component that renders a line between two nodes.

  Props:
  - :edge - Edge data
  - :source-node - Source node data
  - :target-node - Target node data
  - :viewport - Viewport for transformations
  - :on-click - (fn [edge-id event])"
  [{:keys [edge source-node target-node viewport on-click]}]

  (when (and source-node target-node)
    (let [;; Calculate node center points
          source-center (geo/rect-center
                         (geo/rect (:x source-node) (:y source-node)
                                   (:width source-node) (:height source-node)))
          target-center (geo/rect-center
                         (geo/rect (:x target-node) (:y target-node)
                                   (:width target-node) (:height target-node)))

          ;; Convert to screen coordinates
          source-screen (geo/canvas-to-screen viewport (:x source-center) (:y source-center))
          target-screen (geo/canvas-to-screen viewport (:x target-center) (:y target-center))]

      ($ :line
         {:x1 (:x source-screen)
          :y1 (:y source-screen)
          :x2 (:x target-screen)
          :y2 (:y target-screen)
          :stroke "#999"
          :stroke-width 2
          :on-click (fn [e]
                      (when on-click
                        (.stopPropagation e)
                        (on-click (:id edge) e)))}))))
