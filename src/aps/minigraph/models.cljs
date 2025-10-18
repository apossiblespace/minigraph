(ns aps.minigraph.models
  "Data structure definitions for minigraph.

  This namespace defines the core data models used throughout the library:
  - Node: A visual element on the canvas
  - Edge: A connection between two nodes
  - Viewport: The visible area of the canvas
  - Graph: The complete graph state")

;; Node
;; ----

(defn node
  "Create a node with required and optional fields.

  Required:
  - :id - Unique identifier (string)
  - :x - X position on canvas (number)
  - :y - Y position on canvas (number)
  - :width - Node width (number)
  - :height - Node height (number)

  Optional:
  - :type - Node type for styling (keyword, default :default)
  - :data - Custom application data (any, default nil)"
  [{:keys [id x y width height type data]
    :or {type :default}}]
  {:pre [(string? id)
         (number? x)
         (number? y)
         (number? width)
         (number? height)
         (pos? width)
         (pos? height)]}
  {:id id
   :x x
   :y y
   :width width
   :height height
   :type type
   :data data})

(defn node?
  "Check if a value is a valid node."
  [v]
  (and (map? v)
       (string? (:id v))
       (number? (:x v))
       (number? (:y v))
       (number? (:width v))
       (number? (:height v))
       (pos? (:width v))
       (pos? (:height v))
       (keyword? (:type v))))

(defn update-node-position
  "Update a node's position."
  [node x y]
  {:pre [(node? node)]}
  (assoc node :x x :y y))

(defn update-node-size
  "Update a node's size."
  [node width height]
  {:pre [(node? node) (pos? width) (pos? height)]}
  (assoc node :width width :height height))

;; Edge
;; ----

(defn edge
  "Create an edge connecting two nodes.

  Required:
  - :id - Unique identifier (string)
  - :source - Source node ID (string)
  - :target - Target node ID (string)

  Optional:
  - :type - Edge type for styling (keyword, default :default)
  - :data - Custom application data (any, default nil)"
  [{:keys [id source target type data]
    :or {type :default}}]
  {:pre [(string? id)
         (string? source)
         (string? target)
         (not= source target)]}
  {:id id
   :source source
   :target target
   :type type
   :data data})

(defn edge?
  "Check if a value is a valid edge."
  [v]
  (and (map? v)
       (string? (:id v))
       (string? (:source v))
       (string? (:target v))
       (keyword? (:type v))
       (not= (:source v) (:target v))))

;; Viewport
;; --------

(defn viewport
  "Create a viewport representing the visible canvas area.

  Required:
  - :width - Viewport width in pixels (number)
  - :height - Viewport height in pixels (number)

  Optional:
  - :x - X offset (number, default 0)
  - :y - Y offset (number, default 0)
  - :zoom - Zoom level (number, default 1.0, where 1.0 = 100%)"
  [{:keys [x y zoom width height]
    :or {x 0 y 0 zoom 1.0}}]
  {:pre [(number? width)
         (number? height)
         (pos? width)
         (pos? height)
         (number? zoom)
         (pos? zoom)]}
  {:x x
   :y y
   :zoom zoom
   :width width
   :height height})

(defn viewport?
  "Check if a value is a valid viewport."
  [v]
  (and (map? v)
       (number? (:x v))
       (number? (:y v))
       (number? (:zoom v))
       (number? (:width v))
       (number? (:height v))
       (pos? (:zoom v))
       (pos? (:width v))
       (pos? (:height v))))

(defn update-viewport-zoom
  "Update viewport zoom level."
  [viewport zoom]
  {:pre [(viewport? viewport) (pos? zoom)]}
  (assoc viewport :zoom zoom))

(defn update-viewport-pan
  "Update viewport pan offset."
  [viewport x y]
  {:pre [(viewport? viewport)]}
  (assoc viewport :x x :y y))

;; Graph
;; -----

(defn graph
  "Create a graph state containing nodes, edges, and viewport.

   Optional:
  - :nodes - Vector of nodes (default [])
  - :edges - Vector of edges (default [])
  - :viewport - Viewport state (default nil, will be set by canvas component)"
  [{:keys [nodes edges viewport]
    :or {nodes [] edges []}}]
  {:nodes (vec nodes)
   :edges (vec edges)
   :viewport viewport})

(defn graph?
  "Check if a value is a valid graph."
  [v]
  (and (map? v)
       (vector? (:nodes v))
       (vector? (:edges v))
       (or (nil? (:viewport v))
           (viewport? (:viewport v)))))

(defn find-node
  "Find a node by ID in a graph."
  [graph node-id]
  {:pre [(graph? graph) (string? node-id)]}
  (some #(when (= (:id %) node-id) %) (:nodes graph)))

(defn find-edge
  "Find an edge by ID in a graph."
  [graph edge-id]
  {:pre [(graph? graph) (string? edge-id)]}
  (some #(when (= (:id %) edge-id) %) (:edges graph)))

(defn find-edges-between
  "Find all edges between two nodes (in either direction).
   Returns a vector of edges where source/target match the given node IDs."
  [graph node-id-1 node-id-2]
  {:pre [(graph? graph) (string? node-id-1) (string? node-id-2)]}
  (filterv (fn [edge]
             (or (and (= (:source edge) node-id-1)
                      (= (:target edge) node-id-2))
                 (and (= (:source edge) node-id-2)
                      (= (:target edge) node-id-1))))
           (:edges graph)))

(defn edge-exists?
  "Check if an edge already exists between two nodes (in either direction)."
  [graph node-id-1 node-id-2]
  {:pre [(graph? graph) (string? node-id-1) (string? node-id-2)]}
  (boolean (seq (find-edges-between graph node-id-1 node-id-2))))

(defn find-edges-for-nodes
  "Find all edges connected to any of the given node IDs.
   Returns a vector of edge IDs."
  [graph node-ids]
  {:pre [(graph? graph) (coll? node-ids)]}
  (let [node-id-set (set node-ids)]
    (->> (:edges graph)
         (filter (fn [edge]
                   (or (contains? node-id-set (:source edge))
                       (contains? node-id-set (:target edge)))))
         (mapv :id))))

(defn add-node
  "Add a node to the graph. Returns nil if a node with the same ID already exists."
  [graph node]
  {:pre [(graph? graph) (node? node)]}
  (when-not (find-node graph (:id node))
    (update graph :nodes conj node)))

(defn add-edge
  "Add an edge to the graph. Returns nil if:
   - An edge with the same ID already exists
   - Source or target nodes don't exist in the graph"
  [graph edge]
  {:pre [(graph? graph) (edge? edge)]}
  (when (and (not (find-edge graph (:id edge)))
             (find-node graph (:source edge))
             (find-node graph (:target edge)))
    (update graph :edges conj edge)))

(defn update-node
  "Update a node in the graph by ID."
  [graph node-id f & args]
  {:pre [(graph? graph) (string? node-id)]}
  (update graph :nodes
          (fn [nodes]
            (mapv #(if (= (:id %) node-id)
                     (apply f % args)
                     %)
                  nodes))))

(defn remove-node
  "Remove a node and all connected edges from the graph."
  [graph node-id]
  {:pre [(graph? graph) (string? node-id)]}
  (-> graph
      (update :nodes (fn [nodes] (filterv #(not= (:id %) node-id) nodes)))
      (update :edges (fn [edges] (filterv #(and (not= (:source %) node-id)
                                                (not= (:target %) node-id))
                                          edges)))))

(defn remove-edge
  "Remove an edge from the graph."
  [graph edge-id]
  {:pre [(graph? graph) (string? edge-id)]}
  (update graph :edges (fn [edges] (filterv #(not= (:id %) edge-id) edges))))
