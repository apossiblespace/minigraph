(ns aps.minigraph.uix.canvas
  "Canvas component - Main SVG container for graph visualization."
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [aps.minigraph.core :as c]
            [aps.minigraph.geometry :as geo]
            [aps.minigraph.models :as m]
            [aps.minigraph.uix.node :refer [node]]
            [aps.minigraph.uix.edge :refer [edge]]
            [aps.minigraph.uix.controls :refer [controls]]))

;; Helper functions
;; ----------------

(defn- sort-nodes-by-selection
  "Sort nodes so selected nodes render last (appear on top in SVG)."
  [nodes selected-ids]
  (let [{unselected false selected true}
        (group-by #(contains? selected-ids (:id %)) nodes)]
    (concat unselected selected)))

(defn- apply-drag-offset
  "Apply drag offset to a node during dragging."
  [node dx dy]
  (assoc node
         :x (+ (:x node) dx)
         :y (+ (:y node) dy)))

(defn- calculate-hover-target
  "Determine if hovering over a valid edge creation target.
   Returns node-id if valid, nil otherwise."
  [nodes viewport svg-x svg-y source-id graph]
  (when-let [hovered-node (c/node-at-point nodes viewport
                                           svg-x svg-y)]
    (let [target-id (:id hovered-node)
          valid?    (and (not= target-id source-id)
                         (not (m/edge-exists? graph
                                              source-id
                                              target-id)))]
      (when valid? target-id))))

(defn- get-svg-coords
  "Convert mouse event to SVG-relative coordinates.
   Handles events from both child elements and the SVG canvas itself."
  [e]
  (let [;; If currentTarget is a child, get owner SVG; else use it
        svg-elem (or (.-ownerSVGElement (.-currentTarget e))
                     (.-currentTarget e))
        svg-rect (.getBoundingClientRect svg-elem)
        svg-x    (- (.-clientX e) (.-left svg-rect))
        svg-y    (- (.-clientY e) (.-top svg-rect))]
    {:x svg-x :y svg-y}))

(defn- handle-node-drag-move
  "Handle mouse move during node drag."
  [drag-state viewport e on-node-drag]
  (let [{:keys [dx dy]}
        (c/calculate-drag-delta
         viewport
         (:start-x drag-state)
         (:start-y drag-state)
         (.-clientX e)
         (.-clientY e))
        dragged-node-ids (:dragged-node-ids drag-state)]
    {:drag-state  (assoc drag-state :dx dx :dy dy)
     :callback-fn #(when on-node-drag
                     ;; Call on-node-drag for each dragged node
                     (doseq [node-id dragged-node-ids]
                       (on-node-drag node-id dx dy)))}))

(defn- handle-edge-create-move
  "Handle mouse move during edge creation."
  [drag-state nodes viewport graph e]
  (let [{:keys [x y]} (get-svg-coords e)
        hover-target  (calculate-hover-target
                       nodes viewport x y
                       (:source-id drag-state) graph)]
    {:drag-state  (assoc drag-state
                         :current-x x
                         :current-y y
                         :hover-target-id hover-target)
     :callback-fn nil}))

(defn- handle-pan-move
  "Handle mouse move during pan."
  [drag-state viewport e]
  (let [screen-dx    (- (.-clientX e) (:start-x drag-state))
        screen-dy    (- (.-clientY e) (:start-y drag-state))
        new-viewport (c/pan-viewport viewport screen-dx screen-dy)]
    {:viewport   new-viewport
     :drag-state (assoc drag-state
                        :start-x (.-clientX e)
                        :start-y (.-clientY e))}))

(defn- handle-select-move
  "Handle mouse move during drag selection."
  [drag-state e]
  (let [{:keys [x y]} (get-svg-coords e)]
    {:drag-state (assoc drag-state
                        :current-x x
                        :current-y y)}))

(defn- handle-node-drag-end
  "Handle mouse up after node drag."
  [drag-state _viewport node-map _e on-node-drag-end]
  (let [dx               (or (:dx drag-state) 0)
        dy               (or (:dy drag-state) 0)
        dragged-node-ids (:dragged-node-ids drag-state)]
    (when (and on-node-drag-end (or (not= dx 0) (not= dy 0)))
      (doseq [node-id dragged-node-ids]
        (when-let [node (get node-map node-id)]
          (on-node-drag-end node-id
                            (+ (:x node) dx)
                            (+ (:y node) dy)))))))

(defn- handle-edge-create-end
  "Handle mouse up after edge creation."
  [drag-state nodes viewport graph e on-edge-create]
  (let [{:keys [x y]} (get-svg-coords e)
        source-id     (:source-id drag-state)
        target-node   (c/node-at-point nodes viewport x y)
        target-id     (:id target-node)]
    (when (and target-id
               (not= source-id target-id)
               (not (m/edge-exists? graph source-id target-id)))
      (let [edge-id   (str "edge-" source-id "-" target-id)
            edge-data (m/edge {:id     edge-id
                               :source source-id
                               :target target-id})]
        (when on-edge-create
          (on-edge-create edge-data))))))

(defn- handle-select-end
  "Handle mouse up after drag selection."
  [drag-state nodes viewport e selected-nodes set-selected-nodes!
   set-selected-edges!]
  (let [{:keys [x y]} (get-svg-coords e)
        start-x       (:start-x drag-state)
        start-y       (:start-y drag-state)
        ;; Create selection box rect
        select-rect   (geo/selection-box-rect start-x start-y x y)
        ;; Find nodes in selection box
        nodes-in-box  (c/nodes-in-rect nodes viewport select-rect)
        node-ids      (into #{} (map :id nodes-in-box))
        ;; Check if Ctrl/Cmd is held
        ctrl-key      (or (.-ctrlKey e) (.-metaKey e))
        ;; Update selection
        new-selection (if ctrl-key
                        (into selected-nodes node-ids)
                        node-ids)]
    (set-selected-nodes! new-selection)
    ;; Clear edge selection when not holding Ctrl/Cmd
    (when-not ctrl-key
      (set-selected-edges! #{}))))

(defn- zoom-at-point
  "Zoom viewport at a specific point with given factor."
  [viewport zoom-factor point-x point-y]
  (c/zoom-viewport viewport zoom-factor point-x point-y))

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
  - :on-edge-create - (fn [edge-data] -> boolean) Return false to cancel
  - :on-canvas-click - (fn [canvas-x canvas-y event])
  - :on-viewport-change - (fn [viewport])
  - :on-nodes-deleted - (fn [node-ids]) Called with set of node IDs
  - :on-edges-deleted - (fn [edge-ids]) Called with set of edge IDs"
  [{:keys [graph width height
           on-node-click on-node-drag on-node-drag-end
           on-edge-click on-edge-create _on-canvas-click on-viewport-change
           on-nodes-deleted on-edges-deleted]}]

  ;; Initialize viewport if not present in graph
  (let [initial-viewport         (or (:viewport graph)
                                     (m/viewport {:width width :height height}))
        [viewport set-viewport!] (uix/use-state initial-viewport)

        ;; Drag state:
        ;; {:type :node/:pan/:edge-create/:select
        ;;  :node-id string (for :node - the node clicked)
        ;;  :dragged-node-ids set (for :node - all nodes being dragged)
        ;;  :start-x number
        ;;  :start-y number
        ;;  :dx number (for :node)
        ;;  :dy number (for :node)
        ;;  :source-id string (for :edge-create)
        ;;  :current-x number (for :edge-create and :select)
        ;;  :current-y number (for :edge-create and :select)
        ;;  :hover-target-id string (for :edge-create)}
        [drag-state set-drag-state!] (uix/use-state nil)

        ;; Selection state: sets of selected node and edge IDs
        [selected-nodes set-selected-nodes!] (uix/use-state #{})
        [selected-edges set-selected-edges!] (uix/use-state #{})

        ;; Ref to SVG element for focus management
        svg-ref (uix/use-ref nil)

        ;; Ref to track if we just finished dragging (to suppress click)
        just-dragged-ref (uix/use-ref false)

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
        display-node-map
        (if (and drag-state
                 (= (:type drag-state) :node)
                 (:dx drag-state)
                 (:dragged-node-ids drag-state))
          ;; Apply offset to all dragged nodes
          (reduce (fn [acc-map node-id]
                    (if-let [node (get node-map node-id)]
                      (assoc acc-map node-id
                             (apply-drag-offset node
                                                (:dx drag-state)
                                                (:dy drag-state)))
                      acc-map))
                  node-map
                  (:dragged-node-ids drag-state))
          node-map)

        ;; Sort nodes so selected nodes render last (on top)
        sorted-nodes (sort-nodes-by-selection nodes selected-nodes)

        handle-node-click
        (fn [node-id e]
          ;; Suppress click if we just finished dragging
          (if @just-dragged-ref
            (reset! just-dragged-ref false)
            (do
              ;; Focus SVG to enable keyboard events
              (when @svg-ref
                (.focus @svg-ref))
              ;; Also call user's handler if provided
              (when on-node-click
                (on-node-click node-id e)))))

        handle-edge-click
        (fn [edge-id e]
          (.stopPropagation e)
          ;; Focus SVG to enable keyboard events
          (when @svg-ref
            (.focus @svg-ref))
          (let [ctrl-key      (or (.-ctrlKey e) (.-metaKey e))
                new-selection (c/toggle-selection
                               selected-edges
                               edge-id
                               ctrl-key)]
            (set-selected-edges! new-selection)
            ;; Clear node selection when not holding Ctrl/Cmd
            (when-not ctrl-key
              (set-selected-nodes! #{})))
          ;; Also call user's handler if provided
          (when on-edge-click
            (on-edge-click edge-id e)))

        handle-node-mouse-down
        (fn [node-id e is-border-click?]
          (.preventDefault e)
          ;; Focus SVG to enable keyboard events
          (when @svg-ref
            (.focus @svg-ref))
          (if is-border-click?
            ;; Start edge creation from border
            ;; Get the SVG element (parent of the g/rect elements)
            (let [{:keys [x y]} (get-svg-coords e)]
              (set-drag-state! {:type      :edge-create
                                :source-id node-id
                                :start-x   (.-clientX e)
                                :start-y   (.-clientY e)
                                :current-x x
                                :current-y y}))
            ;; Normal node drag from center
            ;; Update selection on mouse down
            (let [ctrl-key          (or (.-ctrlKey e) (.-metaKey e))
                  already-selected? (contains? selected-nodes node-id)
                  ;; Selection logic:
                  ;; - If already selected and no Ctrl -> keep selection
                  ;; - If not selected and no Ctrl -> select only this
                  ;; - If Ctrl -> toggle
                  new-selection     (if ctrl-key
                                      (c/toggle-selection selected-nodes
                                                          node-id
                                                          true)
                                      (if already-selected?
                                        selected-nodes
                                        #{node-id}))
                  ;; Determine which nodes to drag
                  nodes-to-drag     (if already-selected?
                                      ;; Dragging a selected node -> move all
                                      selected-nodes
                                      ;; Dragging unselected node -> move just it
                                      new-selection)]
              (set-selected-nodes! new-selection)
              ;; Clear edge selection when not holding Ctrl/Cmd
              (when-not ctrl-key
                (set-selected-edges! #{}))
              ;; Start drag state with all nodes to be dragged
              (set-drag-state! {:type             :node
                                :node-id          node-id
                                :dragged-node-ids nodes-to-drag
                                :start-x          (.-clientX e)
                                :start-y          (.-clientY e)}))))

        handle-canvas-mouse-down
        (fn [e]
          ;; Only handle if clicking directly on the SVG (not on nodes/edges)
          (when (= (.-target e) (.-currentTarget e))
            (.preventDefault e)
            (let [{:keys [x y]} (get-svg-coords e)
                  shift-key?    (.-shiftKey e)]
              (if shift-key?
                ;; Shift + drag = drag selection
                (set-drag-state! {:type      :select
                                  :start-x   x
                                  :start-y   y
                                  :current-x x
                                  :current-y y})
                ;; Normal canvas drag = pan
                (do
                  ;; Clear both selections unless Ctrl/Cmd is held
                  (when-not (or (.-ctrlKey e) (.-metaKey e))
                    (set-selected-nodes! #{})
                    (set-selected-edges! #{}))
                  ;; Start pan drag
                  (set-drag-state! {:type    :pan
                                    :start-x (.-clientX e)
                                    :start-y (.-clientY e)}))))))

        handle-mouse-move
        (fn [e]
          (when drag-state
            (case (:type drag-state)
              :node
              (let [{:keys [drag-state callback-fn]}
                    (handle-node-drag-move drag-state viewport
                                           e on-node-drag)]
                (set-drag-state! drag-state)
                (when callback-fn (callback-fn)))

              :edge-create
              (let [{:keys [drag-state]}
                    (handle-edge-create-move drag-state nodes
                                             viewport graph e)]
                (set-drag-state! drag-state))

              :pan
              (let [{:keys [viewport drag-state]}
                    (handle-pan-move drag-state viewport e)]
                (set-viewport! viewport)
                (set-drag-state! drag-state))

              :select
              (let [{:keys [drag-state]}
                    (handle-select-move drag-state e)]
                (set-drag-state! drag-state))

              nil)))

        handle-mouse-up
        (fn [e]
          (when drag-state
            (case (:type drag-state)
              :node
              (do
                (handle-node-drag-end drag-state viewport node-map
                                      e on-node-drag-end)
                ;; Mark that we just dragged to suppress next click
                (let [dx (or (:dx drag-state) 0)
                      dy (or (:dy drag-state) 0)]
                  (when (or (not= dx 0) (not= dy 0))
                    (reset! just-dragged-ref true))))

              :edge-create
              (handle-edge-create-end drag-state nodes viewport
                                      graph e on-edge-create)

              :pan
              nil ;; Pan is already complete, nothing more to do

              :select
              (handle-select-end drag-state nodes viewport e
                                 selected-nodes set-selected-nodes!
                                 set-selected-edges!)

              nil)
            (set-drag-state! nil)))

        handle-wheel
        (fn [e]
          (.preventDefault e)
          (let [svg-rect     (.getBoundingClientRect
                              (.-currentTarget e))
                ;; Get mouse position relative to SVG
                screen-x     (- (.-clientX e) (.-left svg-rect))
                screen-y     (- (.-clientY e) (.-top svg-rect))
                ;; Zoom multiplier (negative deltaY = zoom in)
                ;; Use exponential scaling for smooth zooming
                zoom-factor  (js/Math.pow 0.999 (.-deltaY e))
                new-viewport (zoom-at-point viewport zoom-factor
                                            screen-x screen-y)]
            (set-viewport! new-viewport)))

        handle-zoom-in
        (fn []
          ;; Zoom in to center (1.2 = 20% zoom in)
          (let [center-x     (/ width 2)
                center-y     (/ height 2)
                new-viewport (zoom-at-point viewport 1.2
                                            center-x center-y)]
            (set-viewport! new-viewport)))

        handle-zoom-out
        (fn []
          ;; Zoom out from center (0.8 = 20% zoom out)
          (let [center-x     (/ width 2)
                center-y     (/ height 2)
                new-viewport (zoom-at-point viewport 0.8
                                            center-x center-y)]
            (set-viewport! new-viewport)))

        handle-fit
        (fn []
          ;; Fit all nodes into viewport
          (let [new-viewport (c/fit-viewport viewport nodes)]
            (set-viewport! new-viewport)))

        handle-delete
        (fn []
          ;; Delete selected nodes and edges
          (when (or (seq selected-nodes) (seq selected-edges))
            (if (seq selected-nodes)
              ;; Delete nodes (edges cascade automatically in remove-node)
              (when on-nodes-deleted
                (on-nodes-deleted selected-nodes))
              ;; Delete only edges (no nodes selected)
              (when on-edges-deleted
                (on-edges-deleted selected-edges)))
            ;; Clear selection
            (set-selected-nodes! #{})
            (set-selected-edges! #{})))

        handle-key-down
        (fn [e]
          ;; Handle Delete and Backspace keys
          (when (or (= (.-key e) "Delete")
                    (= (.-key e) "Backspace"))
            (.preventDefault e)
            (handle-delete)))]

    ($ :div
       {:style {:position "relative"
                :width    (str width "px")
                :height   (str height "px")}}

       ($ :svg
          {:ref            svg-ref
           :width          width
           :height         height
           :style          {:border      "1px solid #ccc"
                            :background  "#fff"
                            :user-select "none"
                            :outline     "none"}
           :tab-index      0
           :on-mouse-down  handle-canvas-mouse-down
           :on-mouse-move  handle-mouse-move
           :on-mouse-up    handle-mouse-up
           :on-mouse-leave handle-mouse-up
           :on-wheel       handle-wheel
           :on-key-down    handle-key-down}

          ;; Render edges first (so they appear behind nodes)
          (for [e edges]
            ($ edge
               {:key         (:id e)
                :edge        e
                :source-node (get display-node-map (:source e))
                :target-node (get display-node-map (:target e))
                :selected?   (contains? selected-edges (:id e))
                :viewport    viewport
                :on-click    handle-edge-click}))

          ;; Render temporary edge during edge creation
          (when (and drag-state (= (:type drag-state) :edge-create))
            (let [source-node     (get node-map
                                       (:source-id drag-state))
                  hover-target-id (:hover-target-id drag-state)
                  target-node     (when hover-target-id
                                    (get node-map hover-target-id))
                  cursor-pos      {:x (:current-x drag-state)
                                   :y (:current-y drag-state)}]
              ($ edge
                 {:source-node source-node
                  :target-node target-node
                  :cursor-pos  cursor-pos
                  :temporary?  true
                  :viewport    viewport})))

          ;; Render selection box during drag selection
          (when (and drag-state (= (:type drag-state) :select))
            (let [start-x    (:start-x drag-state)
                  start-y    (:start-y drag-state)
                  current-x  (:current-x drag-state)
                  current-y  (:current-y drag-state)
                  min-x      (min start-x current-x)
                  min-y      (min start-y current-y)
                  box-width  (js/Math.abs (- current-x start-x))
                  box-height (js/Math.abs (- current-y start-y))]
              ($ :rect
                 {:x                min-x
                  :y                min-y
                  :width            box-width
                  :height           box-height
                  :fill             "rgba(0, 123, 255, 0.1)"
                  :stroke           "rgba(0, 123, 255, 0.5)"
                  :stroke-width     1
                  :stroke-dasharray "4 2"
                  :pointer-events   "none"})))

          ;; Render nodes with selected nodes last (on top)
          (for [n sorted-nodes]
            (let [;; Use display node map which has drag offset applied
                  display-node (get display-node-map (:id n) n)
                  ;; Highlight if valid edge creation target
                  is-highlighted?
                  (and drag-state
                       (= (:type drag-state) :edge-create)
                       (= (:hover-target-id drag-state)
                          (:id n)))]
              ($ node
                 {:key           (:id n)
                  :node          display-node
                  :viewport      viewport
                  :selected?     (contains? selected-nodes (:id n))
                  :highlight?    is-highlighted?
                  :on-click      handle-node-click
                  :on-mouse-down handle-node-mouse-down}))))

       ;; Render controls overlay
       ($ controls
          {:on-zoom-in  handle-zoom-in
           :on-zoom-out handle-zoom-out
           :on-fit      handle-fit}))))
