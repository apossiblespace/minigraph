(ns aps.minigraph.geometry-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [aps.minigraph.geometry :as geo]))

;; Helper for approximate equality (for floating point comparisons)
(defn approx=
  [a b epsilon]
  (< (Math/abs (- a b)) epsilon))

;; Point operations tests
;; ----------------------

(deftest point-test
  (testing "Creates point"
    (let [p (geo/point 10 20)]
      (is (= 10 (:x p)))
      (is (= 20 (:y p))))))

(deftest distance-test
  (testing "Calculates distance between points"
    (is (= 5.0 (geo/distance 0 0 3 4)))  ; 3-4-5 triangle
    (is (= 0.0 (geo/distance 5 5 5 5)))  ; Same point
    (is (approx= 7.071 (geo/distance 0 0 5 5) 0.01))))  ; sqrt(50)

(deftest point-distance-test
  (testing "Calculates distance between point maps"
    (is (= 5.0 (geo/point-distance {:x 0 :y 0} {:x 3 :y 4})))))

;; Rectangle operations tests
;; ---------------------------

(deftest rect-test
  (testing "Creates rectangle"
    (let [r (geo/rect 10 20 100 80)]
      (is (= 10 (:x r)))
      (is (= 20 (:y r)))
      (is (= 100 (:width r)))
      (is (= 80 (:height r))))))

(deftest rect-contains-point?-test
  (testing "Point inside rectangle"
    (let [r (geo/rect 0 0 100 100)]
      (is (geo/rect-contains-point? r 50 50))
      (is (geo/rect-contains-point? r 0 0))    ; Top-left corner
      (is (geo/rect-contains-point? r 100 100)))) ; Bottom-right corner

  (testing "Point outside rectangle"
    (let [r (geo/rect 0 0 100 100)]
      (is (not (geo/rect-contains-point? r -1 50)))
      (is (not (geo/rect-contains-point? r 50 101)))
      (is (not (geo/rect-contains-point? r 101 101))))))

(deftest rect-intersects?-test
  (testing "Rectangles intersect"
    (let [r1 (geo/rect 0 0 100 100)
          r2 (geo/rect 50 50 100 100)]
      (is (geo/rect-intersects? r1 r2))))

  (testing "Rectangles don't intersect"
    (let [r1 (geo/rect 0 0 100 100)
          r2 (geo/rect 200 200 100 100)]
      (is (not (geo/rect-intersects? r1 r2)))))

  (testing "Rectangles touch at edge"
    (let [r1 (geo/rect 0 0 100 100)
          r2 (geo/rect 100 0 100 100)]
      (is (geo/rect-intersects? r1 r2)))))

(deftest rect-center-test
  (testing "Calculates rectangle center"
    (let [r (geo/rect 0 0 100 100)
          center (geo/rect-center r)]
      (is (= 50 (:x center)))
      (is (= 50 (:y center))))

    (let [r (geo/rect 10 20 80 60)
          center (geo/rect-center r)]
      (is (= 50 (:x center)))
      (is (= 50 (:y center))))))

(deftest rect-bounds-test
  (testing "Calculates bounding box of multiple rectangles"
    (let [r1 (geo/rect 0 0 50 50)
          r2 (geo/rect 100 100 50 50)
          bounds (geo/rect-bounds [r1 r2])]
      (is (= 0 (:x bounds)))
      (is (= 0 (:y bounds)))
      (is (= 150 (:width bounds)))
      (is (= 150 (:height bounds)))))

  (testing "Returns nil for empty list"
    (is (nil? (geo/rect-bounds [])))))

;; Line operations tests
;; ---------------------

(deftest line-length-test
  (testing "Calculates line length"
    (is (= 5.0 (geo/line-length 0 0 3 4)))
    (is (= 0.0 (geo/line-length 5 5 5 5)))))

(deftest point-to-line-distance-test
  (testing "Distance to line segment - perpendicular"
    ;; Point (5, 5) to horizontal line from (0, 0) to (10, 0)
    (is (= 5.0 (geo/point-to-line-distance 5 5 0 0 10 0))))

  (testing "Distance to line segment - endpoint"
    ;; Point (15, 0) to horizontal line from (0, 0) to (10, 0)
    ;; Closest point is endpoint (10, 0)
    (is (= 5.0 (geo/point-to-line-distance 15 0 0 0 10 0))))

  (testing "Distance to zero-length line (point)"
    ;; Line from (5, 5) to (5, 5) is just a point
    (is (= 5.0 (geo/point-to-line-distance 0 5 5 5 5 5)))))

;; Bezier curve operations tests
;; ------------------------------

(deftest cubic-bezier-point-test
  (testing "Start of curve (t=0)"
    (is (= 0 (geo/cubic-bezier-point 0.0 0 10 20 30))))

  (testing "End of curve (t=1)"
    (is (= 30 (geo/cubic-bezier-point 1.0 0 10 20 30))))

  (testing "Middle of curve (t=0.5)"
    (let [result (geo/cubic-bezier-point 0.5 0 10 20 30)]
      (is (approx= 15.0 result 0.1)))))

(deftest simple-bezier-control-points-test
  (testing "Generates control points"
    (let [[p1x p1y p2x p2y] (geo/simple-bezier-control-points 0 0 100 0)]
      ;; Control points should be 40% along the line
      (is (= 40.0 p1x))
      (is (= 0 p1y))
      (is (= 60.0 p2x))
      (is (= 0 p2y)))))

;; Angle and rotation tests
;; -------------------------

(deftest angle-between-points-test
  (testing "Angle from origin to point"
    (is (approx= 0.0 (geo/angle-between-points 0 0 1 0) 0.01))  ; 0 degrees (east)
    (is (approx= (/ Math/PI 2) (geo/angle-between-points 0 0 0 1) 0.01))  ; 90 degrees (south)
    (is (approx= Math/PI (Math/abs (geo/angle-between-points 0 0 -1 0)) 0.01))))  ; 180 degrees (west)

(deftest rotate-point-test
  (testing "Rotate 90 degrees around origin"
    (let [rotated (geo/rotate-point 1 0 0 0 (/ Math/PI 2))]
      (is (approx= 0.0 (:x rotated) 0.01))
      (is (approx= 1.0 (:y rotated) 0.01))))

  (testing "Rotate 180 degrees around origin"
    (let [rotated (geo/rotate-point 1 0 0 0 Math/PI)]
      (is (approx= -1.0 (:x rotated) 0.01))
      (is (approx= 0.0 (:y rotated) 0.01))))

  (testing "No rotation (0 degrees)"
    (let [rotated (geo/rotate-point 5 5 0 0 0)]
      (is (= 5 (:x rotated)))
      (is (= 5 (:y rotated))))))

;; Viewport transformation tests
;; ------------------------------

(deftest screen-to-canvas-test
  (testing "Identity transformation (no pan, zoom 1.0)"
    (let [viewport {:x 0 :y 0 :zoom 1.0}
          canvas (geo/screen-to-canvas viewport 100 200)]
      (is (= 100 (:x canvas)))
      (is (= 200 (:y canvas)))))

  (testing "With zoom 2.0"
    (let [viewport {:x 0 :y 0 :zoom 2.0}
          canvas (geo/screen-to-canvas viewport 100 200)]
      (is (= 50.0 (:x canvas)))
      (is (= 100.0 (:y canvas)))))

  (testing "With pan offset"
    (let [viewport {:x 50 :y 100 :zoom 1.0}
          canvas (geo/screen-to-canvas viewport 100 200)]
      (is (= 150 (:x canvas)))
      (is (= 300 (:y canvas)))))

  (testing "With pan and zoom"
    (let [viewport {:x 50 :y 100 :zoom 2.0}
          canvas (geo/screen-to-canvas viewport 100 200)]
      (is (= 100.0 (:x canvas)))
      (is (= 200.0 (:y canvas))))))

(deftest canvas-to-screen-test
  (testing "Identity transformation (no pan, zoom 1.0)"
    (let [viewport {:x 0 :y 0 :zoom 1.0}
          screen (geo/canvas-to-screen viewport 100 200)]
      (is (= 100 (:x screen)))
      (is (= 200 (:y screen)))))

  (testing "With zoom 2.0"
    (let [viewport {:x 0 :y 0 :zoom 2.0}
          screen (geo/canvas-to-screen viewport 50 100)]
      (is (= 100.0 (:x screen)))
      (is (= 200.0 (:y screen)))))

  (testing "With pan offset"
    (let [viewport {:x 50 :y 100 :zoom 1.0}
          screen (geo/canvas-to-screen viewport 150 300)]
      (is (= 100 (:x screen)))
      (is (= 200 (:y screen)))))

  (testing "Round-trip transformation"
    (let [viewport {:x 25 :y 50 :zoom 1.5}
          canvas (geo/screen-to-canvas viewport 100 200)
          screen (geo/canvas-to-screen viewport (:x canvas) (:y canvas))]
      (is (approx= 100 (:x screen) 0.01))
      (is (approx= 200 (:y screen) 0.01)))))

(deftest viewport-bounds-test
  (testing "Viewport bounds with zoom 1.0"
    (let [viewport {:x 0 :y 0 :zoom 1.0 :width 800 :height 600}
          bounds (geo/viewport-bounds viewport)]
      (is (= 0 (:x bounds)))
      (is (= 0 (:y bounds)))
      (is (= 800 (:width bounds)))
      (is (= 600 (:height bounds)))))

  (testing "Viewport bounds with zoom 2.0"
    (let [viewport {:x 0 :y 0 :zoom 2.0 :width 800 :height 600}
          bounds (geo/viewport-bounds viewport)]
      (is (= 0 (:x bounds)))
      (is (= 0 (:y bounds)))
      (is (= 400.0 (:width bounds)))
      (is (= 300.0 (:height bounds)))))

  (testing "Viewport bounds with pan offset"
    (let [viewport {:x 100 :y 200 :zoom 1.0 :width 800 :height 600}
          bounds (geo/viewport-bounds viewport)]
      (is (= 100 (:x bounds)))
      (is (= 200 (:y bounds))))))
