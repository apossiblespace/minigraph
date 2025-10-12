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

;; Viewport operations
;; -------------------

(defn zoom-viewport
  "Zoom viewport by a factor, centered on a point.
   zoom-delta is multiplier (e.g., 1.1 for 10% zoom in, 0.9 for 10% zoom out).
   center-x, center-y are the screen coordinates to zoom toward."
  [viewport zoom-delta center-x center-y]
  {:pre [(m/viewport? viewport) (pos? zoom-delta)]}
  (let [{:keys [x y zoom]} viewport
        new-zoom (max min-zoom (min max-zoom (* zoom zoom-delta)))
        zoom-ratio (/ new-zoom zoom)
        ;; Calculate canvas point at center before zoom
        canvas-center (geo/screen-to-canvas viewport center-x center-y)
        ;; Calculate new offset to keep canvas-center at same screen position
        new-x (- (:x canvas-center) (/ center-x new-zoom))
        new-y (- (:y canvas-center) (/ center-y new-zoom))]
    (m/update-viewport-pan
     (m/update-viewport-zoom viewport new-zoom)
     new-x new-y)))

(defn pan-viewport
  "Pan viewport by screen pixel delta."
  [viewport dx dy]
  {:pre [(m/viewport? viewport)]}
  (let [{:keys [x y zoom]} viewport
        ;; Convert screen delta to canvas delta
        canvas-dx (/ dx zoom)
        canvas-dy (/ dy zoom)]
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
    ;; Calculate bounds of all nodes
    (let [node-rects (map (fn [node]
                            (geo/rect (:x node) (:y node)
                                      (:width node) (:height node)))
                          nodes)
          bounds (geo/rect-bounds node-rects)
          padding fit-viewport-padding
          content-width (+ (:width bounds) (* 2 padding))
          content-height (+ (:height bounds) (* 2 padding))
          {:keys [width height]} viewport
          zoom-x (/ width content-width)
          zoom-y (/ height content-height)
          new-zoom (max min-zoom (min max-zoom (min zoom-x zoom-y)))
          new-x (- (:x bounds) padding)
          new-y (- (:y bounds) padding)]
      (m/update-viewport-pan
       (m/update-viewport-zoom viewport new-zoom)
       new-x new-y))))

;; Hit detection
;; -------------

(defn node-at-point
  "Find the topmost node at screen coordinates.
   Returns node or nil."
  [nodes viewport screen-x screen-y]
  {:pre [(vector? nodes) (m/viewport? viewport)]}
  (let [canvas-point (geo/screen-to-canvas viewport screen-x screen-y)
        cx (:x canvas-point)
        cy (:y canvas-point)]
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
  (let [canvas-point (geo/screen-to-canvas viewport screen-x screen-y)
        cx (:x canvas-point)
        cy (:y canvas-point)
        ;; Build node lookup map
        node-map (into {} (map (fn [n] [(:id n) n]) nodes))
        ;; Convert threshold from screen pixels to canvas units
        canvas-threshold (/ threshold (:zoom viewport))]
    ;; Find first edge whose line is within threshold of point
    (some (fn [edge]
            (let [source-node (get node-map (:source edge))
                  target-node (get node-map (:target edge))]
              (when (and source-node target-node)
                (let [source-center (geo/rect-center
                                     (geo/rect (:x source-node) (:y source-node)
                                               (:width source-node) (:height source-node)))
                      target-center (geo/rect-center
                                     (geo/rect (:x target-node) (:y target-node)
                                               (:width target-node) (:height target-node)))
                      dist (geo/point-to-line-distance
                            cx cy
                            (:x source-center) (:y source-center)
                            (:x target-center) (:y target-center))]
                  (when (<= dist canvas-threshold)
                    edge)))))
          (reverse edges))))

(defn nodes-in-rect
  "Find all nodes that intersect with a selection rectangle.
   Selection rect is in screen coordinates."
  [nodes viewport screen-rect]
  {:pre [(vector? nodes) (m/viewport? viewport) (map? screen-rect)]}
  (let [;; Convert screen rect to canvas rect
        top-left (geo/screen-to-canvas viewport (:x screen-rect) (:y screen-rect))
        bottom-right (geo/screen-to-canvas viewport
                                           (+ (:x screen-rect) (:width screen-rect))
                                           (+ (:y screen-rect) (:height screen-rect)))
        canvas-rect (geo/rect (:x top-left) (:y top-left)
                              (- (:x bottom-right) (:x top-left))
                              (- (:y bottom-right) (:y top-left)))]
    (filterv (fn [node]
               (geo/rect-intersects?
                canvas-rect
                (geo/rect (:x node) (:y node) (:width node) (:height node))))
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
        end-canvas (geo/screen-to-canvas viewport screen-end-x screen-end-y)]
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
