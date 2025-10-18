(ns aps.minigraph.uix.canvas
  "Canvas component - Main SVG container for graph visualization."
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [aps.minigraph.core :as c]
            [aps.minigraph.models :as m]
            [aps.minigraph.uix.node :refer [node]]
            [aps.minigraph.uix.edge :refer [edge]]
            [aps.minigraph.uix.controls :refer [controls]]))

(defui canvas
  "Main graph canvas component.

  Props:
  - :graph - Graph data structure (required)
  - :width - Canvas width in pixels (required)
  - :height - Canvas height in pixels (required)
  - :on-node-click - (fn [node-id event])
  - :on-node-drag - (fn [node-id dx dy])
  - :on-node-drag-end - (fn [node-id x y])
  - :on-edge-click - (fn [edge-id event])
  - :on-canvas-click - (fn [canvas-x canvas-y event])
  - :on-viewport-change - (fn [viewport])"
  [{:keys [graph width height
           on-node-click on-node-drag on-node-drag-end
           on-edge-click _on-canvas-click on-viewport-change]}]

  ;; Initialize viewport if not present in graph
  (let [initial-viewport         (or (:viewport graph)
                                     (m/viewport {:width width :height height}))
        [viewport set-viewport!] (uix/use-state initial-viewport)

        ;; Drag state:
        ;; 
        ;; {:type :node/:pan
        ;;  :node-id string
        ;;  :start-x number
        ;;  :start-y number
        ;;  :dx number
        ;;  :dy number}
        [drag-state set-drag-state!] (uix/use-state nil)

        ;; Selection state: set of selected node IDs
        [selected-nodes set-selected-nodes!] (uix/use-state #{})

        ;; Notify parent of viewport changes
        _ (uix/use-effect
           (fn []
             (when on-viewport-change
               (on-viewport-change viewport)))
           [on-viewport-change viewport])

        nodes    (:nodes graph)
        edges    (:edges graph)
        node-map (into {} (map (fn [n] [(:id n) n]) nodes))

        ;; Create display node map with drag offset applied
        display-node-map (if (and drag-state
                                  (= (:type drag-state) :node)
                                  (:dx drag-state))
                           (let [dragged-id   (:node-id drag-state)
                                 dragged-node (get node-map dragged-id)
                                 updated-node (when dragged-node
                                                (assoc dragged-node
                                                       :x (+ (:x dragged-node) (:dx drag-state))
                                                       :y (+ (:y dragged-node) (:dy drag-state))))]
                             (if updated-node
                               (assoc node-map dragged-id updated-node)
                               node-map))
                           node-map)

        ;; Sort nodes so selected nodes render last (appear on top in SVG)
        sorted-nodes (let [{unselected false selected true} (group-by #(contains? selected-nodes (:id %)) nodes)]
                       (concat unselected selected))

        handle-node-click
        (fn [node-id e]
          ;; Also call user's handler if provided
          (when on-node-click
            (on-node-click node-id e)))

        handle-node-mouse-down
        (fn [node-id e]
          (.preventDefault e)
          ;; Update selection on mouse down, this ensures the node gets selected
          ;; styling immediately when starting a drag
          (let [ctrl-key      (or (.-ctrlKey e) (.-metaKey e))
                new-selection (c/toggle-selection selected-nodes node-id ctrl-key)]
            (set-selected-nodes! new-selection))
          ;; Start drag state
          (set-drag-state! {:type    :node
                            :node-id node-id
                            :start-x (.-clientX e)
                            :start-y (.-clientY e)}))

        handle-canvas-mouse-down
        (fn [e]
          ;; Only handle if clicking directly on the SVG (not on nodes/edges)
          (when (= (.-target e) (.-currentTarget e))
            (.preventDefault e)
            ;; Clear selection unless Ctrl/Cmd is held
            (when-not (or (.-ctrlKey e) (.-metaKey e))
              (set-selected-nodes! #{}))
            ;; Start pan drag
            (set-drag-state! {:type    :pan
                              :start-x (.-clientX e)
                              :start-y (.-clientY e)})))

        handle-mouse-move
        (fn [e]
          (when drag-state
            (case (:type drag-state)
              :node
              (let [{:keys [dx dy]} (c/calculate-drag-delta viewport
                                                            (:start-x drag-state)
                                                            (:start-y drag-state)
                                                            (.-clientX e)
                                                            (.-clientY e))]
                ;; Store the delta in drag-state for visual feedback
                (set-drag-state! (assoc drag-state :dx dx :dy dy))
                (when on-node-drag
                  (on-node-drag (:node-id drag-state) dx dy)))

              :pan
              (let [screen-dx    (- (.-clientX e) (:start-x drag-state))
                    screen-dy    (- (.-clientY e) (:start-y drag-state))
                    ;; pan-viewport handles the conversion from screen to canvas units
                    new-viewport (c/pan-viewport viewport screen-dx screen-dy)]
                (set-viewport! new-viewport)
                ;; Update drag start position for next move
                (set-drag-state! (assoc drag-state
                                        :start-x (.-clientX e)
                                        :start-y (.-clientY e))))

              nil)))

        handle-mouse-up
        (fn [e]
          (when drag-state
            (case (:type drag-state)
              :node
              (let [node            (get node-map (:node-id drag-state))
                    {:keys [dx dy]} (c/calculate-drag-delta viewport
                                                            (:start-x drag-state)
                                                            (:start-y drag-state)
                                                            (.-clientX e)
                                                            (.-clientY e))]
                (when (and node on-node-drag-end)
                  (on-node-drag-end (:node-id drag-state)
                                    (+ (:x node) dx)
                                    (+ (:y node) dy))))

              :pan
              nil ;; Pan is already complete, nothing more to do

              nil)
            (set-drag-state! nil)))

        handle-wheel
        (fn [e]
          (.preventDefault e)
          (let [svg-rect     (.getBoundingClientRect (.-currentTarget e))
                ;; Get mouse position relative to SVG
                screen-x     (- (.-clientX e) (.-left svg-rect))
                screen-y     (- (.-clientY e) (.-top svg-rect))
                ;; Calculate zoom multiplier (negative deltaY = zoom in)
                ;; Use exponential scaling for smooth zooming
                zoom-factor  (js/Math.pow 0.999 (.-deltaY e))
                new-viewport (c/zoom-viewport viewport zoom-factor screen-x screen-y)]
            (set-viewport! new-viewport)))

        handle-zoom-in
        (fn []
          ;; Zoom in towards center of viewport (1.2 = 20% zoom in)
          (let [center-x     (/ width 2)
                center-y     (/ height 2)
                new-viewport (c/zoom-viewport viewport 1.2 center-x center-y)]
            (set-viewport! new-viewport)))

        handle-zoom-out
        (fn []
          ;; Zoom out from center of viewport (0.8 = 20% zoom out)
          (let [center-x     (/ width 2)
                center-y     (/ height 2)
                new-viewport (c/zoom-viewport viewport 0.8 center-x center-y)]
            (set-viewport! new-viewport)))

        handle-fit
        (fn []
          ;; Fit all nodes into viewport
          (let [new-viewport (c/fit-viewport viewport nodes)]
            (set-viewport! new-viewport)))]

    ($ :div
       {:style {:position "relative"
                :width    (str width "px")
                :height   (str height "px")}}

       ($ :svg
          {:width          width
           :height         height
           :style          {:border      "1px solid #ccc"
                            :background  "#fff"
                            :user-select "none"}
           :on-mouse-down  handle-canvas-mouse-down
           :on-mouse-move  handle-mouse-move
           :on-mouse-up    handle-mouse-up
           :on-mouse-leave handle-mouse-up
           :on-wheel       handle-wheel}

          ;; Render edges first (so they appear behind nodes)
          (for [e edges]
            ($ edge
               {:key         (:id e)
                :edge        e
                :source-node (get display-node-map (:source e))
                :target-node (get display-node-map (:target e))
                :viewport    viewport
                :on-click    on-edge-click}))

          ;; Render nodes with selected nodes last (on top)
          (for [n sorted-nodes]
            (let [;; Apply drag offset if this node is being dragged
                  is-dragging? (and drag-state
                                    (= (:type drag-state) :node)
                                    (= (:node-id drag-state) (:id n)))
                  display-node (if (and is-dragging? (:dx drag-state))
                                 (assoc n
                                        :x (+ (:x n) (:dx drag-state))
                                        :y (+ (:y n) (:dy drag-state)))
                                 n)]
              ($ node
                 {:key           (:id n)
                  :node          display-node
                  :viewport      viewport
                  :selected?     (contains? selected-nodes (:id n))
                  :on-click      handle-node-click
                  :on-mouse-down handle-node-mouse-down}))))

       ;; Render controls overlay
       ($ controls
          {:on-zoom-in  handle-zoom-in
           :on-zoom-out handle-zoom-out
           :on-fit      handle-fit}))))
