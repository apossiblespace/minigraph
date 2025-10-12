(ns aps.minigraph.uix.node
  "Node component - Renders graph nodes with drag support."
  (:require [uix.core :as uix :refer [defui $]]
            [aps.minigraph.geometry :as geo]))

(defui node
  "Node component that renders a rectangle with optional label.

  Props:
  - :node - Node data
  - :viewport - Viewport for transformations
  - :selected? - Boolean, whether node is selected
  - :on-click - (fn [node-id event])
  - :on-drag-start - (fn [node-id event])
  - :on-drag - (fn [node-id dx dy])
  - :on-drag-end - (fn [node-id x y])"
  [{:keys [node viewport selected? on-click on-mouse-down]}]

  (let [;; Convert canvas coordinates to screen coordinates
        screen-pos (geo/canvas-to-screen viewport (:x node) (:y node))
        screen-width (* (:width node) (:zoom viewport))
        screen-height (* (:height node) (:zoom viewport))

        label (get-in node [:data :label] (:id node))]

    ($ :g
       {:on-click (fn [e]
                    (when on-click
                      (.stopPropagation e)
                      (on-click (:id node) e)))}

       ;; Node rectangle
       ($ :rect
          {:x (:x screen-pos)
           :y (:y screen-pos)
           :width screen-width
           :height screen-height
           :fill (if selected? "#e3f2fd" "#fff")
           :stroke (if selected? "#2196f3" "#666")
           :stroke-width (if selected? 3 2)
           :rx 4
           :cursor "move"
           :on-mouse-down (fn [e]
                            (when on-mouse-down
                              (.stopPropagation e)
                              (on-mouse-down (:id node) e)))})

       ;; Node label
       ($ :text
          {:x (+ (:x screen-pos) (/ screen-width 2))
           :y (+ (:y screen-pos) (/ screen-height 2))
           :text-anchor "middle"
           :dominant-baseline "middle"
           :fill "#333"
           :font-size 14
           :pointer-events "none"}
          label))))
