(ns aps.minigraph.uix.controls
  "Controls component - Zoom and fit controls for the canvas."
  (:require [uix.core :as uix :refer [defui $]]))

(defui controls
  "Zoom and fit controls component.

  Props:
  - :on-zoom-in - (fn []) Called when zoom in button is clicked
  - :on-zoom-out - (fn []) Called when zoom out button is clicked
  - :on-fit - (fn []) Called when fit button is clicked
  - :style - Optional CSS style map for positioning"
  [{:keys [on-zoom-in on-zoom-out on-fit style]}]

  (let [button-style {:padding "8px 12px"
                      :margin "4px"
                      :border "1px solid #ccc"
                      :border-radius "4px"
                      :background "#fff"
                      :cursor "pointer"
                      :font-size "14px"
                      :user-select "none"}
        container-style (merge {:display "flex"
                                :flex-direction "column"
                                :position "absolute"
                                :top "10px"
                                :right "10px"
                                :z-index 1000}
                               style)]

    ($ :div
       {:style container-style}

       ($ :button
          {:style button-style
           :on-click (fn [e]
                       (.preventDefault e)
                       (when on-zoom-in (on-zoom-in)))
           :title "Zoom In"}
          "+")

       ($ :button
          {:style button-style
           :on-click (fn [e]
                       (.preventDefault e)
                       (when on-zoom-out (on-zoom-out)))
           :title "Zoom Out"}
          "−")

       ($ :button
          {:style button-style
           :on-click (fn [e]
                       (.preventDefault e)
                       (when on-fit (on-fit)))
           :title "Fit to View"}
          "⊡"))))
