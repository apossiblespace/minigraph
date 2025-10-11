(ns aps.minigraph.core-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [aps.minigraph.core :as c]
            [aps.minigraph.models :as m]
            [aps.minigraph.geometry :as geo]))

;; Helper for approximate equality
(defn approx=
  [a b epsilon]
  (< (Math/abs (- a b)) epsilon))

;; Viewport operations tests
;; --------------------------

(deftest zoom-viewport-test
  (testing "Zoom in centered on viewport center"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          zoomed (c/zoom-viewport vp 1.5 400 300)]
      (is (= 1.5 (:zoom zoomed)))))

  (testing "Zoom is clamped to max"
    (let [vp (m/make-viewport {:width 800 :height 600 :zoom 4.0})
          zoomed (c/zoom-viewport vp 2.0 400 300)]
      (is (= 5.0 (:zoom zoomed)))))  ; Clamped to max 5.0

  (testing "Zoom is clamped to min"
    (let [vp (m/make-viewport {:width 800 :height 600 :zoom 0.2})
          zoomed (c/zoom-viewport vp 0.3 400 300)]
      (is (= 0.1 (:zoom zoomed))))))  ; Clamped to min 0.1

(deftest pan-viewport-test
  (testing "Pan by screen pixels"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          panned (c/pan-viewport vp 100 50)]
      (is (= -100 (:x panned)))
      (is (= -50 (:y panned)))))

  (testing "Pan with zoom affects canvas delta"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 2.0})
          panned (c/pan-viewport vp 100 50)]
      ;; At 2x zoom, 100px screen = 50px canvas
      (is (= -50.0 (:x panned)))
      (is (= -25.0 (:y panned))))))

(deftest fit-viewport-test
  (testing "Fit viewport to nodes"
    (let [vp (m/make-viewport {:width 800 :height 600})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          n2 (m/make-node {:id "n2" :x 300 :y 300 :width 100 :height 100})
          fitted (c/fit-viewport vp [n1 n2])]
      ;; Should zoom to fit both nodes with padding
      (is (< (:zoom fitted) 1.0))  ; Zoomed out
      (is (<= (:x fitted) 0))       ; Pan includes first node
      (is (>= (:zoom fitted) 0.1)))) ; Within min zoom

  (testing "Fit viewport with no nodes resets view"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 100 :y 200 :zoom 0.5})
          fitted (c/fit-viewport vp [])]
      (is (= 1.0 (:zoom fitted)))
      (is (= 0 (:x fitted)))
      (is (= 0 (:y fitted))))))

;; Hit detection tests
;; --------------------

(deftest node-at-point-test
  (testing "Finds node at point"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          n2 (m/make-node {:id "n2" :x 200 :y 200 :width 100 :height 100})
          found (c/node-at-point [n1 n2] vp 50 50)]
      (is (= "n1" (:id found)))))

  (testing "Returns nil when no node at point"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          found (c/node-at-point [n1] vp 500 500)]
      (is (nil? found))))

  (testing "Returns topmost node when overlapping"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          n2 (m/make-node {:id "n2" :x 50 :y 50 :width 100 :height 100})
          found (c/node-at-point [n1 n2] vp 75 75)]
      (is (= "n2" (:id found))))) ; n2 is later in vector, so on top

  (testing "Works with zoomed viewport"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 2.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          ;; At 2x zoom, canvas point (50, 50) is at screen point (100, 100)
          found (c/node-at-point [n1] vp 100 100)]
      (is (= "n1" (:id found))))))

(deftest edge-at-point-test
  (testing "Finds edge near point"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          n2 (m/make-node {:id "n2" :x 200 :y 200 :width 100 :height 100})
          e1 (m/make-edge {:id "e1" :source "n1" :target "n2"})
          ;; Click near the line between node centers
          found (c/edge-at-point [e1] [n1 n2] vp 125 125 10)]
      (is (= "e1" (:id found)))))

  (testing "Returns nil when point too far from edge"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          n2 (m/make-node {:id "n2" :x 200 :y 200 :width 100 :height 100})
          e1 (m/make-edge {:id "e1" :source "n1" :target "n2"})
          found (c/edge-at-point [e1] [n1 n2] vp 500 500 10)]
      (is (nil? found)))))

(deftest nodes-in-rect-test
  (testing "Finds nodes in selection rectangle"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          n2 (m/make-node {:id "n2" :x 200 :y 200 :width 100 :height 100})
          n3 (m/make-node {:id "n3" :x 500 :y 500 :width 100 :height 100})
          selection-rect {:x 0 :y 0 :width 300 :height 300}
          selected (c/nodes-in-rect [n1 n2 n3] vp selection-rect)]
      (is (= 2 (count selected)))
      (is (some #(= "n1" (:id %)) selected))
      (is (some #(= "n2" (:id %)) selected))))

  (testing "Returns empty when no nodes in rectangle"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          n1 (m/make-node {:id "n1" :x 0 :y 0 :width 100 :height 100})
          selection-rect {:x 500 :y 500 :width 100 :height 100}
          selected (c/nodes-in-rect [n1] vp selection-rect)]
      (is (empty? selected)))))

;; Selection operations tests
;; ---------------------------

(deftest toggle-selection-test
  (testing "Toggle without ctrl replaces selection"
    (let [selected (c/toggle-selection #{} "n1" false)]
      (is (= #{"n1"} selected)))

    (let [selected (c/toggle-selection #{"n1"} "n2" false)]
      (is (= #{"n2"} selected))))

  (testing "Toggle with ctrl adds to selection"
    (let [selected (c/toggle-selection #{} "n1" true)]
      (is (= #{"n1"} selected)))

    (let [selected (c/toggle-selection #{"n1"} "n2" true)]
      (is (= #{"n1" "n2"} selected))))

  (testing "Toggle with ctrl removes if already selected"
    (let [selected (c/toggle-selection #{"n1" "n2"} "n1" true)]
      (is (= #{"n2"} selected)))))

(deftest add-to-selection-test
  (testing "Add single item"
    (let [selected (c/add-to-selection #{} "n1")]
      (is (= #{"n1"} selected))))

  (testing "Add multiple items"
    (let [selected (c/add-to-selection #{"n1"} ["n2" "n3"])]
      (is (= #{"n1" "n2" "n3"} selected)))))

(deftest remove-from-selection-test
  (testing "Remove single item"
    (let [selected (c/remove-from-selection #{"n1" "n2"} "n1")]
      (is (= #{"n2"} selected))))

  (testing "Remove multiple items"
    (let [selected (c/remove-from-selection #{"n1" "n2" "n3"} ["n1" "n2"])]
      (is (= #{"n3"} selected)))))

(deftest select-all-test
  (testing "Select all nodes"
    (let [n1 (m/make-node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2 (m/make-node {:id "n2" :x 0 :y 0 :width 10 :height 10})
          selected (c/select-all [n1 n2])]
      (is (= #{"n1" "n2"} selected)))))

(deftest clear-selection-test
  (testing "Clear selection"
    (is (= #{} (c/clear-selection)))))

;; Drag calculations tests
;; ------------------------

(deftest calculate-drag-delta-test
  (testing "Calculate drag delta at zoom 1.0"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 1.0})
          delta (c/calculate-drag-delta vp 100 100 150 200)]
      (is (= 50 (:dx delta)))
      (is (= 100 (:dy delta)))))

  (testing "Calculate drag delta at zoom 2.0"
    (let [vp (m/make-viewport {:width 800 :height 600 :x 0 :y 0 :zoom 2.0})
          delta (c/calculate-drag-delta vp 100 100 150 200)]
      ;; At 2x zoom, 50px screen = 25px canvas
      (is (= 25.0 (:dx delta)))
      (is (= 50.0 (:dy delta))))))

(deftest apply-drag-to-node-test
  (testing "Apply drag to single node"
    (let [node (m/make-node {:id "n1" :x 100 :y 200 :width 50 :height 50})
          dragged (c/apply-drag-to-node node 10 20)]
      (is (= 110 (:x dragged)))
      (is (= 220 (:y dragged)))
      (is (= "n1" (:id dragged)))))  ; ID unchanged

  (testing "Drag with negative delta"
    (let [node (m/make-node {:id "n1" :x 100 :y 200 :width 50 :height 50})
          dragged (c/apply-drag-to-node node -50 -100)]
      (is (= 50 (:x dragged)))
      (is (= 100 (:y dragged))))))

(deftest apply-drag-to-nodes-test
  (testing "Apply drag to multiple selected nodes"
    (let [n1 (m/make-node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2 (m/make-node {:id "n2" :x 100 :y 100 :width 10 :height 10})
          n3 (m/make-node {:id "n3" :x 200 :y 200 :width 10 :height 10})
          dragged (c/apply-drag-to-nodes [n1 n2 n3] #{"n1" "n3"} 50 50)]
      (is (= 50 (:x (nth dragged 0))))   ; n1 moved
      (is (= 100 (:x (nth dragged 1))))  ; n2 not moved
      (is (= 250 (:x (nth dragged 2)))))) ; n3 moved

  (testing "Nodes not in selection are unchanged"
    (let [n1 (m/make-node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2 (m/make-node {:id "n2" :x 100 :y 100 :width 10 :height 10})
          dragged (c/apply-drag-to-nodes [n1 n2] #{"n1"} 50 50)]
      (is (= 100 (:x (nth dragged 1))))
      (is (= 100 (:y (nth dragged 1)))))))

(deftest constrain-node-position-test
  (testing "Node position currently unconstrained"
    (let [node (m/make-node {:id "n1" :x 100 :y 200 :width 50 :height 50})
          constrained (c/constrain-node-position node nil)]
      ;; For now, just returns node as-is
      (is (= node constrained)))))
