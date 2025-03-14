(ns ui.components.graph-overview-ai
  (:require [reagent.core :as r]
            [applied-science.js-interop :as j]
            [ui.actions.graph-overview-ai :refer [create-chat-ui-blocks-for-selected-overview]]
            [ui.utils :refer [p button-with-tooltip]]
            ["@blueprintjs/core" :as bp :refer [ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]))


(defn filtered-pages-button []
  (let [disabled? (r/atom true)]
    (fn []
      (let [mutation-callback (fn mutation-callback [mutations observer]
                                (doseq [mutation mutations]
                                  (when (= (.-type mutation) "childList")
                                    (doseq [node (array-seq (.-addedNodes mutation))]
                                      (let [parent-class (j/get node :className)
                                            child-class (j/get-in node [:firstChild :className])]
                                        (when (and (instance? js/Element node)
                                                (or (= "roam-body-main"
                                                      parent-class)
                                                  (= "rm-graph-view"
                                                    parent-class)))
                                          (if (or (= "rm-graph-view"
                                                    child-class)
                                                (= "rm-graph-view"
                                                  parent-class))
                                            (do
                                              (p "ON Graph Overview Page, enable the *Load filtered pages into chat* button")
                                              (reset! disabled? false))
                                            (reset! disabled? true))))))))
            star-observing    (let [observer (js/MutationObserver. mutation-callback)]
                                (.observe observer js/document #js {:childList true
                                                                    :subtree true}))])
      [button-with-tooltip
       "Filter pages from Graph Overview and use them as context for chat with llm. You can refine the context by adjusting the filters in Graph Overview."
       [:> Button {:class-name "chat-with-filtered-pages"
                   :minimal true
                   :small true
                   :disabled @disabled?
                   :on-click (fn []
                               (p "*Load filtered pages into chat* :button clicked")
                               (let [chat-block-uid (j/call-in js/window [:roamAlphaAPI :util :generateUID])
                                     context-block-uid (j/call-in js/window [:roamAlphaAPI :util :generateUID])]
                                 ;(p "chat-block-uid" chat-block-uid context-block-uid)
                                 (create-chat-ui-blocks-for-selected-overview
                                   chat-block-uid
                                   context-block-uid)))}
        "Chat with selected pages"]])))
