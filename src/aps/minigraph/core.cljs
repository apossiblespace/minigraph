(ns aps.minigraph.core
  "Core graph operations for minigraph.

  Pure functions for:
  - Viewport transformations and navigation
  - Hit detection (finding nodes/edges at points)
  - Selection operations
  - Drag and position calculations"
  (:require [aps.minigraph.geometry :as geo]
            [aps.minigraph.models :as m]))

;; Constants
;; ---------

(def ^:const min-zoom
  "Minimum allowed zoom level"
  0.1)

(def ^:const max-zoom
  "Maximum allowed zoom level"
  5.0)

(def ^:const fit-viewport-padding
  "Default padding (in canvas units) when fitting viewport to nodes"
  50)

;; Helper functions
;; -----------------

(defn- clamp-zoom
  "Clamp zoom level to valid range."
  [zoom]
  (max min-zoom (min max-zoom zoom)))

(defn- node-center-point
  "Get the center point of a node."
  [node]
  (geo/rect-center
   (geo/rect (:x node) (:y node) (:width node) (:height node))))

(defn- build-node-map
  "Build a map of node IDs to nodes for O(1) lookup."
  [nodes]
  (into {} (map (fn [n] [(:id n) n]) nodes)))

(defn- calculate-zoom-offset
  "Calculate viewport offset to keep canvas point centered at screen position during zoom."
  [canvas-point screen-x screen-y new-zoom]
  {:x (- (:x canvas-point) (/ screen-x new-zoom))
   :y (- (:y canvas-point) (/ screen-y new-zoom))})

(defn- calculate-fit-zoom
  "Calculate zoom level to fit content within viewport dimensions."
  [content-width content-height viewport-width viewport-height]
  (let [zoom-x (/ viewport-width content-width)
        zoom-y (/ viewport-height content-height)]
    (clamp-zoom (min zoom-x zoom-y))))

(defn- nodes-bounding-box
  "Calculate bounding box for a collection of nodes with padding."
  [nodes padding]
  (let [node-rects (map (fn [node]
                          (geo/rect (:x node) (:y node)
                                    (:width node) (:height node)))
                        nodes)
        bounds     (geo/rect-bounds node-rects)]
    {:bounds         bounds
     :content-width  (+ (:width bounds) (* 2 padding))
     :content-height (+ (:height bounds) (* 2 padding))
     :offset-x       (- (:x bounds) padding)
     :offset-y       (- (:y bounds) padding)}))

(defn- edge-endpoints
  "Get the center points of an edge's source and target nodes.
   Returns {:source point :target point} or nil if nodes not found."
  [edge node-map]
  (when-let [source-node (get node-map (:source edge))]
    (when-let [target-node (get node-map (:target edge))]
      {:source (node-center-point source-node)
       :target (node-center-point target-node)})))

(defn- point-near-edge?
  "Check if a canvas point is within threshold distance of an edge line."
  [cx cy edge node-map canvas-threshold]
  (when-let [endpoints (edge-endpoints edge node-map)]
    (let [{:keys [source target]} endpoints
          dist                    (geo/point-to-line-distance
                                   cx cy
                                   (:x source) (:y source)
                                   (:x target) (:y target))]
      (<= dist canvas-threshold))))

(defn- screen-rect-to-canvas
  "Convert a screen-space rectangle to canvas-space coordinates."
  [viewport screen-rect]
  (let [top-left     (geo/screen-to-canvas viewport (:x screen-rect) (:y screen-rect))
        bottom-right (geo/screen-to-canvas viewport
                                           (+ (:x screen-rect) (:width screen-rect))
                                           (+ (:y screen-rect) (:height screen-rect)))]
    (geo/rect (:x top-left) (:y top-left)
              (- (:x bottom-right) (:x top-left))
              (- (:y bottom-right) (:y top-left)))))

(defn- node-rect
  "Create a rect from a node's position and dimensions."
  [node]
  (geo/rect (:x node) (:y node) (:width node) (:height node)))

;; Viewport operations
;; -------------------

(defn zoom-viewport
  "Zoom viewport by a factor, centered on a point.
   zoom-delta is multiplier (e.g., 1.1 for 10% zoom in, 0.9 for 10% zoom out).
   center-x, center-y are the screen coordinates to zoom toward."
  [viewport zoom-delta center-x center-y]
  {:pre [(m/viewport? viewport) (pos? zoom-delta)]}
  (let [{:keys [zoom]} viewport
        new-zoom       (clamp-zoom (* zoom zoom-delta))
        canvas-center  (geo/screen-to-canvas viewport center-x center-y)
        offset         (calculate-zoom-offset canvas-center center-x center-y new-zoom)]
    (m/update-viewport-pan
     (m/update-viewport-zoom viewport new-zoom)
     (:x offset) (:y offset))))

(defn pan-viewport
  "Pan viewport by screen pixel delta."
  [viewport dx dy]
  {:pre [(m/viewport? viewport)]}
  (let [{:keys [x y zoom]} viewport
        ;; Convert screen delta to canvas delta
        canvas-dx          (/ dx zoom)
        canvas-dy          (/ dy zoom)]
    (m/update-viewport-pan viewport
                           (- x canvas-dx)
                           (- y canvas-dy))))

(defn fit-viewport
  "Adjust viewport to fit all nodes with padding.
   If no nodes, resets to default view."
  [viewport nodes]
  {:pre [(m/viewport? viewport) (vector? nodes)]}
  (if (empty? nodes)
    ;; Reset to default view
    (m/update-viewport-pan
     (m/update-viewport-zoom viewport 1.0)
     0 0)
    ;; Calculate fit parameters and update viewport
    (let [{:keys [content-width content-height offset-x offset-y]}
          (nodes-bounding-box nodes fit-viewport-padding)

          {:keys [width height]}
          viewport

          new-zoom
          (calculate-fit-zoom content-width content-height width height)]
      (m/update-viewport-pan
       (m/update-viewport-zoom viewport new-zoom)
       offset-x offset-y))))

;; Hit detection
;; -------------

(defn node-at-point
  "Find the topmost node at screen coordinates.
   Returns node or nil."
  [nodes viewport screen-x screen-y]
  {:pre [(vector? nodes) (m/viewport? viewport)]}
  (let [canvas-point (geo/screen-to-canvas viewport screen-x screen-y)
        cx           (:x canvas-point)
        cy           (:y canvas-point)]
    ;; Find first node that contains the point (checking in reverse order for top-most)
    (some (fn [node]
            (when (geo/rect-contains-point?
                   (geo/rect (:x node) (:y node) (:width node) (:height node))
                   cx cy)
              node))
          (reverse nodes))))

(defn edge-at-point
  "Find the topmost edge at screen coordinates within threshold pixels.
   Returns edge or nil. threshold is the maximum distance in screen pixels."
  [edges nodes viewport screen-x screen-y threshold]
  {:pre [(vector? edges) (vector? nodes) (m/viewport? viewport) (pos? threshold)]}
  (let [canvas-point     (geo/screen-to-canvas viewport screen-x screen-y)
        cx               (:x canvas-point)
        cy               (:y canvas-point)
        node-map         (build-node-map nodes)
        canvas-threshold (/ threshold (:zoom viewport))]
    (some (fn [edge]
            (when (point-near-edge? cx cy edge node-map canvas-threshold)
              edge))
          (reverse edges))))

(defn nodes-in-rect
  "Find all nodes that intersect with a selection rectangle.
   Selection rect is in screen coordinates."
  [nodes viewport screen-rect]
  {:pre [(vector? nodes) (m/viewport? viewport) (map? screen-rect)]}
  (let [canvas-rect (screen-rect-to-canvas viewport screen-rect)]
    (filterv (fn [node]
               (geo/rect-intersects? canvas-rect (node-rect node)))
             nodes)))

;; Selection operations
;; ---------------------

(defn toggle-selection
  "Toggle an item in the selection set.
   If ctrl-key is true, toggles the item. Otherwise, replaces selection."
  [selected-ids item-id ctrl-key]
  {:pre [(set? selected-ids) (string? item-id)]}
  (if ctrl-key
    (if (contains? selected-ids item-id)
      (disj selected-ids item-id)
      (conj selected-ids item-id))
    #{item-id}))

(defn add-to-selection
  "Add item(s) to selection. Accepts single ID or collection of IDs."
  [selected-ids item-ids]
  {:pre [(set? selected-ids)]}
  (if (string? item-ids)
    (conj selected-ids item-ids)
    (into selected-ids item-ids)))

(defn remove-from-selection
  "Remove item(s) from selection. Accepts single ID or collection of IDs."
  [selected-ids item-ids]
  {:pre [(set? selected-ids)]}
  (if (string? item-ids)
    (disj selected-ids item-ids)
    (apply disj selected-ids item-ids)))

(defn select-all
  "Select all nodes."
  [nodes]
  {:pre [(vector? nodes)]}
  (into #{} (map :id nodes)))

(defn clear-selection
  "Clear all selections."
  []
  #{})

;; Drag calculations
;; ------------------

(defn calculate-drag-delta
  "Calculate canvas drag delta from screen coordinates.
   Returns {:dx :dy} in canvas units."
  [viewport screen-start-x screen-start-y screen-end-x screen-end-y]
  {:pre [(m/viewport? viewport)]}
  (let [start-canvas (geo/screen-to-canvas viewport screen-start-x screen-start-y)
        end-canvas   (geo/screen-to-canvas viewport screen-end-x screen-end-y)]
    {:dx (- (:x end-canvas) (:x start-canvas))
     :dy (- (:y end-canvas) (:y start-canvas))}))

(defn apply-drag-to-node
  "Apply drag delta to a node's position."
  [node dx dy]
  {:pre [(m/node? node)]}
  (m/update-node-position node
                          (+ (:x node) dx)
                          (+ (:y node) dy)))

(defn apply-drag-to-nodes
  "Apply drag delta to multiple nodes.
   node-ids is a set of node IDs to drag."
  [nodes node-ids dx dy]
  {:pre [(vector? nodes) (set? node-ids)]}
  (mapv (fn [node]
          (if (contains? node-ids (:id node))
            (apply-drag-to-node node dx dy)
            node))
        nodes))

(defn constrain-node-position
  "Constrain node position to stay within canvas bounds (optional).
   For now, just returns the node as-is. Can be extended later
   to enforce boundaries or grid snapping."
  [node _canvas-bounds]
  {:pre [(m/node? node)]}
  node)
