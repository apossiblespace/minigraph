(ns aps.minigraph.models-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [aps.minigraph.models :as m]))

;; Node tests
;; ----------

(deftest node-test
  (testing "Creates valid node with required fields"
    (let [node (m/node {:id     "node-1"
                        :x      100
                        :y      200
                        :width  120
                        :height 80})]
      (is (= "node-1" (:id node)))
      (is (= 100 (:x node)))
      (is (= 200 (:y node)))
      (is (= 120 (:width node)))
      (is (= 80 (:height node)))
      (is (= :default (:type node)))))

  (testing "Creates node with custom type and data"
    (let [node (m/node {:id     "node-2"
                        :x      0
                        :y      0
                        :width  100
                        :height 100
                        :type   :custom
                        :data   {:label "My Node"}})]
      (is (= :custom (:type node)))
      (is (= {:label "My Node"} (:data node)))))

  (testing "Throws on invalid dimensions"
    (is (thrown? js/Error
                 (m/node {:id "bad" :x 0 :y 0 :width 0 :height 100})))
    (is (thrown? js/Error
                 (m/node {:id "bad" :x 0 :y 0 :width 100 :height -10})))))

(deftest node?-test
  (testing "Validates correct nodes"
    (is (m/node? (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10}))))

  (testing "Rejects invalid nodes"
    (is (not (m/node? {:id "n1"}))) ; Missing required fields
    (is (not (m/node? {:id "n1" :x 0 :y 0 :width 10 :height 10 :type "string"}))) ; Wrong type
    (is (not (m/node? nil)))))

(deftest update-node-position-test
  (testing "Updates node position"
    (let [node    (m/node {:id "n1" :x 10 :y 20 :width 50 :height 50})
          updated (m/update-node-position node 100 200)]
      (is (= 100 (:x updated)))
      (is (= 200 (:y updated)))
      (is (= "n1" (:id updated))) ; Other fields unchanged
      (is (= 50 (:width updated))))))

(deftest update-node-size-test
  (testing "Updates node size"
    (let [node    (m/node {:id "n1" :x 10 :y 20 :width 50 :height 50})
          updated (m/update-node-size node 100 80)]
      (is (= 100 (:width updated)))
      (is (= 80 (:height updated)))
      (is (= 10 (:x updated))))) ; Position unchanged

  (testing "Throws on invalid size"
    (let [node (m/node {:id "n1" :x 10 :y 20 :width 50 :height 50})]
      (is (thrown? js/Error (m/update-node-size node 0 50)))
      (is (thrown? js/Error (m/update-node-size node 50 -10))))))

;; Edge tests
;; ----------

(deftest make-edge-test
  (testing "Creates valid edge"
    (let [edge (m/edge {:id     "e1"
                        :source "n1"
                        :target "n2"})]
      (is (= "e1" (:id edge)))
      (is (= "n1" (:source edge)))
      (is (= "n2" (:target edge)))
      (is (= :default (:type edge)))))

  (testing "Creates edge with custom type and data"
    (let [edge (m/edge {:id     "e1"
                        :source "n1"
                        :target "n2"
                        :type   :custom
                        :data   {:label "Connection"}})]
      (is (= :custom (:type edge)))
      (is (= {:label "Connection"} (:data edge)))))

  (testing "Throws on self-loop"
    (is (thrown? js/Error
                 (m/edge {:id "e1" :source "n1" :target "n1"})))))

(deftest edge?-test
  (testing "Validates correct edges"
    (is (m/edge? (m/edge {:id "e1" :source "n1" :target "n2"}))))

  (testing "Rejects invalid edges"
    (is (not (m/edge? {:id "e1"}))) ; Missing fields
    (is (not (m/edge? {:id "e1" :source "n1" :target "n1" :type :default}))) ; Self-loop
    (is (not (m/edge? nil)))))

;; Viewport tests
;; --------------

(deftest make-viewport-test
  (testing "Creates viewport with defaults"
    (let [vp (m/viewport {:width 800 :height 600})]
      (is (= 0 (:x vp)))
      (is (= 0 (:y vp)))
      (is (= 1.0 (:zoom vp)))
      (is (= 800 (:width vp)))
      (is (= 600 (:height vp)))))

  (testing "Creates viewport with custom values"
    (let [vp (m/viewport {:x 100 :y 200 :zoom 0.5 :width 1024 :height 768})]
      (is (= 100 (:x vp)))
      (is (= 200 (:y vp)))
      (is (= 0.5 (:zoom vp)))))

  (testing "Throws on invalid values"
    (is (thrown? js/Error (m/viewport {:width 0 :height 600})))
    (is (thrown? js/Error (m/viewport {:width 800 :height 600 :zoom 0})))))

(deftest viewport?-test
  (testing "Validates correct viewport"
    (is (m/viewport? (m/viewport {:width 800 :height 600}))))

  (testing "Rejects invalid viewport"
    (is (not (m/viewport? {:width 800}))) ; Missing fields
    (is (not (m/viewport? nil)))))

(deftest update-viewport-zoom-test
  (testing "Updates zoom level"
    (let [vp      (m/viewport {:width 800 :height 600})
          updated (m/update-viewport-zoom vp 2.0)]
      (is (= 2.0 (:zoom updated)))
      (is (= 0 (:x updated))))) ; Other fields unchanged

  (testing "Throws on invalid zoom"
    (let [vp (m/viewport {:width 800 :height 600})]
      (is (thrown? js/Error (m/update-viewport-zoom vp 0)))
      (is (thrown? js/Error (m/update-viewport-zoom vp -1))))))

(deftest update-viewport-pan-test
  (testing "Updates pan offset"
    (let [vp      (m/viewport {:width 800 :height 600})
          updated (m/update-viewport-pan vp 100 200)]
      (is (= 100 (:x updated)))
      (is (= 200 (:y updated)))
      (is (= 1.0 (:zoom updated)))))) ; Zoom unchanged

;; Graph tests
;; -----------

(deftest make-graph-test
  (testing "Creates empty graph"
    (let [g (m/graph {})]
      (is (= [] (:nodes g)))
      (is (= [] (:edges g)))
      (is (nil? (:viewport g)))))

  (testing "Creates graph with nodes and edges"
    (let [nodes [(m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})]
          edges [(m/edge {:id "e1" :source "n1" :target "n2"})]
          g (m/graph {:nodes nodes :edges edges})]
      (is (= 1 (count (:nodes g))))
      (is (= 1 (count (:edges g)))))))

(deftest graph?-test
  (testing "Validates correct graph"
    (is (m/graph? (m/graph {}))))

  (testing "Rejects invalid graph"
    (is (not (m/graph? {:nodes nil}))) ; Nodes not a vector
    (is (not (m/graph? nil)))))

(deftest find-node-test
  (testing "Finds existing node"
    (let [node  (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          g     (m/graph {:nodes [node]})
          found (m/find-node g "n1")]
      (is (= node found))))

  (testing "Returns nil for non-existent node"
    (let [g (m/graph {})]
      (is (nil? (m/find-node g "missing"))))))

(deftest find-edge-test
  (testing "Finds existing edge"
    (let [edge  (m/edge {:id "e1" :source "n1" :target "n2"})
          g     (m/graph {:edges [edge]})
          found (m/find-edge g "e1")]
      (is (= edge found))))

  (testing "Returns nil for non-existent edge"
    (let [g (m/graph {})]
      (is (nil? (m/find-edge g "missing"))))))

(deftest add-node-test
  (testing "Adds node to graph"
    (let [g       (m/graph {})
          node    (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          updated (m/add-node g node)]
      (is (= 1 (count (:nodes updated))))
      (is (= node (first (:nodes updated))))))

  (testing "Rejects duplicate node ID"
    (let [n1      (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2      (m/node {:id "n1" :x 50 :y 50 :width 20 :height 20})
          g       (m/graph {:nodes [n1]})
          updated (m/add-node g n2)]
      (is (nil? updated)))))

(deftest add-edge-test
  (testing "Adds edge to graph when nodes exist"
    (let [n1      (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2      (m/node {:id "n2" :x 50 :y 50 :width 10 :height 10})
          g       (m/graph {:nodes [n1 n2]})
          edge    (m/edge {:id "e1" :source "n1" :target "n2"})
          updated (m/add-edge g edge)]
      (is (= 1 (count (:edges updated))))
      (is (= edge (first (:edges updated))))))

  (testing "Rejects duplicate edge ID"
    (let [n1      (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2      (m/node {:id "n2" :x 50 :y 50 :width 10 :height 10})
          e1      (m/edge {:id "e1" :source "n1" :target "n2"})
          g       (m/graph {:nodes [n1 n2] :edges [e1]})
          e2      (m/edge {:id "e1" :source "n2" :target "n1"})
          updated (m/add-edge g e2)]
      (is (nil? updated))))

  (testing "Rejects edge when source node doesn't exist"
    (let [n2      (m/node {:id "n2" :x 50 :y 50 :width 10 :height 10})
          g       (m/graph {:nodes [n2]})
          edge    (m/edge {:id "e1" :source "n1" :target "n2"})
          updated (m/add-edge g edge)]
      (is (nil? updated))))

  (testing "Rejects edge when target node doesn't exist"
    (let [n1      (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          g       (m/graph {:nodes [n1]})
          edge    (m/edge {:id "e1" :source "n1" :target "n2"})
          updated (m/add-edge g edge)]
      (is (nil? updated)))))

(deftest update-node-test
  (testing "Updates node in graph"
    (let [node         (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          g            (m/graph {:nodes [node]})
          updated      (m/update-node g "n1" m/update-node-position 100 200)
          updated-node (m/find-node updated "n1")]
      (is (= 100 (:x updated-node)))
      (is (= 200 (:y updated-node)))))

  (testing "Leaves other nodes unchanged"
    (let [n1      (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2      (m/node {:id "n2" :x 50 :y 50 :width 10 :height 10})
          g       (m/graph {:nodes [n1 n2]})
          updated (m/update-node g "n1" m/update-node-position 100 200)]
      (is (= 50 (:x (m/find-node updated "n2")))))))

(deftest remove-node-test
  (testing "Removes node from graph"
    (let [n1      (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2      (m/node {:id "n2" :x 50 :y 50 :width 10 :height 10})
          g       (m/graph {:nodes [n1 n2]})
          updated (m/remove-node g "n1")]
      (is (= 1 (count (:nodes updated))))
      (is (nil? (m/find-node updated "n1")))
      (is (some? (m/find-node updated "n2")))))

  (testing "Removes connected edges when removing node"
    (let [n1      (m/node {:id "n1" :x 0 :y 0 :width 10 :height 10})
          n2      (m/node {:id "n2" :x 50 :y 50 :width 10 :height 10})
          e1      (m/edge {:id "e1" :source "n1" :target "n2"})
          e2      (m/edge {:id "e2" :source "n2" :target "n1"})
          g       (m/graph {:nodes [n1 n2] :edges [e1 e2]})
          updated (m/remove-node g "n1")]
      (is (= 0 (count (:edges updated)))))))

(deftest remove-edge-test
  (testing "Removes edge from graph"
    (let [e1      (m/edge {:id "e1" :source "n1" :target "n2"})
          e2      (m/edge {:id "e2" :source "n2" :target "n3"})
          g       (m/graph {:edges [e1 e2]})
          updated (m/remove-edge g "e1")]
      (is (= 1 (count (:edges updated))))
      (is (nil? (m/find-edge updated "e1")))
      (is (some? (m/find-edge updated "e2"))))))
