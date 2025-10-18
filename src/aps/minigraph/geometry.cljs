(ns aps.minigraph.geometry
  "Geometry and math utilities for minigraph.

  Functions for:
  - Point and rectangle calculations
  - Bounding box operations
  - Line and curve geometry
  - Distance calculations")

;; Point operations
;; ----------------

(defn point
  "Create a point map."
  [x y]
  {:x x :y y})

(defn distance
  "Calculate Euclidean distance between two points."
  [x1 y1 x2 y2]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))

(defn point-distance
  "Calculate distance between two point maps."
  [p1 p2]
  (distance (:x p1) (:y p1) (:x p2) (:y p2)))

;; Rectangle operations
;; --------------------

(defn rect
  "Create a rectangle map from position and dimensions."
  [x y width height]
  {:x x :y y :width width :height height})

(defn rect-contains-point?
  "Check if a rectangle contains a point."
  [rect px py]
  (let [{:keys [x y width height]} rect]
    (and (>= px x)
         (>= py y)
         (<= px (+ x width))
         (<= py (+ y height)))))

(defn rect-intersects?
  "Check if two rectangles intersect."
  [r1 r2]
  (let [{x1 :x y1 :y w1 :width h1 :height} r1
        {x2 :x y2 :y w2 :width h2 :height} r2]
    (not (or (> x1 (+ x2 w2))
             (< (+ x1 w1) x2)
             (> y1 (+ y2 h2))
             (< (+ y1 h1) y2)))))

(defn rect-center
  "Get the center point of a rectangle."
  [rect]
  (let [{:keys [x y width height]} rect]
    (point (+ x (/ width 2))
           (+ y (/ height 2)))))

(defn rect-bounds
  "Get bounding box of multiple rectangles."
  [rects]
  (when (seq rects)
    (let [x-coords (mapcat (fn [{:keys [x width]}] [x (+ x width)]) rects)
          y-coords (mapcat (fn [{:keys [y height]}] [y (+ y height)]) rects)
          min-x    (apply min x-coords)
          min-y    (apply min y-coords)
          max-x    (apply max x-coords)
          max-y    (apply max y-coords)]
      (rect min-x min-y (- max-x min-x) (- max-y min-y)))))

;; Line operations
;; ---------------

(defn line-length
  "Calculate length of a line segment."
  [x1 y1 x2 y2]
  (distance x1 y1 x2 y2))

(defn point-to-line-distance
  "Calculate perpendicular distance from point to line segment.
   Returns the shortest distance from point (px, py) to the line
   segment from (x1, y1) to (x2, y2)."
  [px py x1 y1 x2 y2]
  (let [dx        (- x2 x1)
        dy        (- y2 y1)
        length-sq (+ (* dx dx) (* dy dy))]
    (if (zero? length-sq)
      ;; Line segment is actually a point
      (distance px py x1 y1)
      ;; Calculate projection parameter
      (let [t      (max 0 (min 1 (/ (+ (* (- px x1) dx)
                                       (* (- py y1) dy))
                                    length-sq)))
            proj-x (+ x1 (* t dx))
            proj-y (+ y1 (* t dy))]
        (distance px py proj-x proj-y)))))

;; Bezier curve operations
;; ------------------------

(defn cubic-bezier-point
  "Calculate a point on a cubic Bezier curve at parameter t (0 to 1).
   P0, P1, P2, P3 are control points."
  [t p0 p1 p2 p3]
  (let [mt  (- 1 t)
        mt2 (* mt mt)
        mt3 (* mt2 mt)
        t2  (* t t)
        t3  (* t2 t)]
    (+ (* mt3 p0)
       (* 3 mt2 t p1)
       (* 3 mt t2 p2)
       (* t3 p3))))

(defn simple-bezier-control-points
  "Calculate simple Bezier control points for a smooth curve
   between two points. Returns [p1x p1y p2x p2y]."
  [x1 y1 x2 y2]
  (let [dx        (- x2 x1)
        _dy       (- y2 y1)
        ;; Control points offset by 70% of the distance for pronounced curves
        offset-x  (* 0.7 dx)
        _offset-y (* 0.7 _dy)]
    [(+ x1 offset-x) y1
     (- x2 offset-x) y2]))

;; Angle and rotation
;; ------------------

(defn angle-between-points
  "Calculate angle in radians from point 1 to point 2."
  [x1 y1 x2 y2]
  (Math/atan2 (- y2 y1) (- x2 x1)))

(defn rotate-point
  "Rotate a point around an origin by angle in radians."
  [px py origin-x origin-y angle]
  (let [cos-a (Math/cos angle)
        sin-a (Math/sin angle)
        dx    (- px origin-x)
        dy    (- py origin-y)]
    (point (+ origin-x (- (* dx cos-a) (* dy sin-a)))
           (+ origin-y (* dx sin-a) (* dy cos-a)))))

;; Viewport transformations
;; -------------------------

(defn screen-to-canvas
  "Transform screen coordinates to canvas coordinates.
   Takes viewport {:x :y :zoom} and screen point."
  [viewport screen-x screen-y]
  (let [{:keys [x y zoom]} viewport]
    (point (+ x (/ screen-x zoom))
           (+ y (/ screen-y zoom)))))

(defn canvas-to-screen
  "Transform canvas coordinates to screen coordinates.
   Takes viewport {:x :y :zoom} and canvas point."
  [viewport canvas-x canvas-y]
  (let [{:keys [x y zoom]} viewport]
    (point (* (- canvas-x x) zoom)
           (* (- canvas-y y) zoom))))

(defn viewport-bounds
  "Get the canvas bounds visible in the viewport."
  [viewport]
  (let [{:keys [x y width height zoom]} viewport
        canvas-width (/ width zoom)
        canvas-height (/ height zoom)]
    (rect x y canvas-width canvas-height)))
