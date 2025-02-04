(ns ui.render-comp.bottom-bar
  (:require [applied-science.js-interop :as j]
            ["@blueprintjs/core" :as bp :refer [Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]
            [ui.components.bottom-bar :as bcomp :refer [bottom-bar-buttons]]
            [ui.utils :refer [p button-with-tooltip]]
            [reagent.core :as r]
            [reagent.dom :as rd]))


(defn bottom-bar [show?]
  (fn []
    (when @show?
     [:div.bottom-bar
      {:style {:display "flex"
               :flex-direction "row"
               :background-color "#eeebeb"
               :justify-content "center"
               :font-size "10px"
               :padding-right "11px"
               :align-items "center"
               :border "1px"}}
      [bottom-bar-buttons]])))


(defn bottom-bar-toggle [toggle-atom]
  [:div.bottom-bar-toggle-button-div
   [button-with-tooltip 
    "Toggle llm bottom bar."
    [:> Button 
     {:class-name "bottom-bar-toggle-button"
      :icon "widget-footer"
      :minimal true 
      :tabIndex 0
      :small true
      :on-click (fn []
                  (do 
                    (swap! toggle-atom not)
                    (p "toggle bottom bar")))}]]])


(defn bottom-bar-main []
  (p "Creating bottom bar")
  (let [parent-el (.querySelector js/document ".roam-body")
        new-child (.createElement js/document "div")
        already-present? (.querySelector js/document ".llm-bottom-bar")
        show-bottom-bar? (r/atom true)
        
        top-bar (.querySelector js/document ".rm-topbar")
        new-icon (.createElement js/document "div")]
      (p "show bottom bar" @show-bottom-bar?)
      (set! (.-className new-child) "llm-bottom-bar")
      (set! (.-className new-icon) "llm-bottom-bar-toggle")
      (.appendChild parent-el new-child)
      (.appendChild top-bar new-icon)
      (rd/render [bottom-bar show-bottom-bar?] new-child)
      (rd/render [bottom-bar-toggle show-bottom-bar?] new-icon)
      (when (and
              (nil? already-present?)
              parent-el)
        (j/assoc-in! parent-el [:style :display] "flex")
        (j/assoc-in! parent-el [:style :flex-direction] "column"))))
       
