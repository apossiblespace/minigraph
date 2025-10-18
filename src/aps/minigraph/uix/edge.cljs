(ns aps.minigraph.uix.edge
  "Edge component - Renders connections between nodes."
  (:require [uix.core :as uix :refer [defui $]]
            [aps.minigraph.geometry :as geo]))

(defui edge
  "Renders an edge between nodes, or a temporary edge during creation.

  Props:
  - :edge - Edge data (optional for temporary edges)
  - :source-node - Source node data (required)
  - :target-node - Target node data (optional, uses cursor-pos if nil)
  - :cursor-pos - Cursor position {:x :y} (for temporary edges)
  - :temporary? - Boolean, render as temporary edge
  - :selected? - Boolean, render as selected
  - :viewport - Viewport for transformations
  - :on-click - (fn [edge-id event])"
  [{:keys [edge source-node target-node cursor-pos temporary? selected?
           viewport on-click]}]

  (when source-node
    (let [;; Calculate source center point
          source-center
          (geo/rect-center
           (geo/rect (:x source-node) (:y source-node)
                     (:width source-node) (:height source-node)))

          ;; Convert source to screen coordinates
          source-screen
          (geo/canvas-to-screen viewport
                                (:x source-center)
                                (:y source-center))

          ;; Determine end point: target node center or cursor position
          end-point
          (if target-node
            ;; Use target node center
            (let [target-center
                  (geo/rect-center
                   (geo/rect (:x target-node) (:y target-node)
                             (:width target-node) (:height target-node)))]
              (geo/canvas-to-screen viewport
                                    (:x target-center)
                                    (:y target-center)))
            ;; Use cursor position (already in screen coords)
            cursor-pos)

          ;; Calculate bezier control points
          [c1x c1y c2x c2y]
          (when end-point
            (geo/simple-bezier-control-points
             (:x source-screen) (:y source-screen)
             (:x end-point) (:y end-point)))

          ;; Create SVG path for cubic bezier curve
          path-d
          (when end-point
            (str "M " (:x source-screen) " " (:y source-screen)
                 " C " c1x " " c1y
                 ", " c2x " " c2y
                 ", " (:x end-point) " " (:y end-point)))

          ;; Determine stroke style based on state
          stroke-color (cond
                         selected? "#2196f3"
                         temporary? "#2196f3"
                         :else "#999")
          stroke-width (if selected? 3 2)
          ;; Dashed if temporary and no target (not snapped)
          stroke-dash (if (and temporary? (not target-node))
                        "5,5"
                        "0")]

      (when path-d
        ($ :path
           {:d path-d
            :stroke stroke-color
            :stroke-width stroke-width
            :stroke-dasharray stroke-dash
            :fill "none"
            :pointer-events (if temporary? "none" "auto")
            :on-click (fn [e]
                        (when (and on-click edge)
                          (.stopPropagation e)
                          (on-click (:id edge) e)))})))))
