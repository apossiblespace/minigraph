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
  - :highlight? - Boolean, whether node is highlighted as valid drop target
  - :on-click - (fn [node-id event])
  - :on-mouse-down - (fn [node-id event is-border-click?])"
  [{:keys [node viewport selected? highlight? on-click on-mouse-down]}]

  (let [;; Convert canvas coordinates to screen coordinates
        screen-pos (geo/canvas-to-screen viewport (:x node) (:y node))
        screen-width (* (:width node) (:zoom viewport))
        screen-height (* (:height node) (:zoom viewport))

        label (get-in node [:data :label] (:id node))

        ;; Border zone is the outer 25% of the node
        border-threshold 0.25

        ;; Check if a mouse event is in the border zone
        is-border-click?
        (fn [e]
          (let [svg-rect (.getBoundingClientRect (.-currentTarget e))
                ;; Get mouse position relative to the rect element
                local-x (- (.-clientX e) (.-left svg-rect))
                local-y (- (.-clientY e) (.-top svg-rect))
                ;; Calculate relative position (0 to 1)
                rel-x (/ local-x screen-width)
                rel-y (/ local-y screen-height)]
            ;; Check if in outer border zone
            (or (< rel-x border-threshold)
                (> rel-x (- 1 border-threshold))
                (< rel-y border-threshold)
                (> rel-y (- 1 border-threshold)))))]

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
           :fill (cond
                   highlight? "#c8e6c9"
                   selected? "#e3f2fd"
                   :else "#fff")
           :stroke (cond
                     highlight? "#4caf50"
                     selected? "#2196f3"
                     :else "#666")
           :stroke-width (cond
                           highlight? 3
                           selected? 3
                           :else 2)
           :rx 4
           :cursor "move"
           :on-mouse-down (fn [e]
                            (when on-mouse-down
                              (.stopPropagation e)
                              (on-mouse-down (:id node) e (is-border-click? e))))})

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
