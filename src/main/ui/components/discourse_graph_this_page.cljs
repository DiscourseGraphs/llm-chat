(ns ui.components.discourse-graph-this-page
  (:require [cljs.core.async.interop :as asy :refer [<p!]]
            [reagent.core :as r :refer [atom]]
            [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]
            [ui.extract-data.chat :as ed :refer [extract-query-pages data-for-nodes get-all-images-for-node]]
            [ui.components.chat :refer [chat-context]]
            [ui.components.chin :refer [chin]]
            [ui.actions.dg-this-page :refer [create-bare-struct ask-llm]]
            [ui.utils :refer [button-popover button-with-tooltip watch-string model-mappings get-safety-settings update-block-string-for-block-with-child settings-button-popover image-to-text-for p get-child-of-child-with-str title->uid q block-has-child-with-str? call-llm-api update-block-string uid->title log get-child-with-str get-child-of-child-with-str-on-page get-open-page-uid get-block-parent-with-order get-focused-block create-struct gen-new-uid default-chat-struct get-todays-uid]]
            [ui.extract-data.create-prompts :refer [manual-prompt-guide]]
            ["@blueprintjs/core" :as bp :refer [ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]))


(defn discourse-graph-this-page-button [block-uid
                                        default-model
                                        default-temp
                                        default-max-tokens
                                        get-linked-refs?
                                        extract-query-pages?
                                        extract-query-pages-ref?
                                        active?
                                        context
                                        pre-prompt]
  (fn [_]
    #_(println "--" (get-child-of-child-with-str-on-page "llm chat" "Quick action buttons" button-name "Context"))
    [:> ButtonGroup
     {:class-name "button-with-settings"
      :style {:overflow "hidden"
              :display "flex"
              :flex-direction "row"
              :justify-content "space-between"
              :align-items "center"
              :flex "1 1 1"}
      :minimal true}
     [:div {:style {:flex "1 1 1"}}
      [button-with-tooltip
       "LLM proposes candidate discourse nodes based on the context of the current page (including zoomed-in pages). "
       [:> Button {:minimal true
                   :small true
                   :loading @active?
                   :on-click (fn [e]
                               (println "clicked discourse graph this page")
                               (when (not @active?)
                                 (reset! active? true))
                               (go
                                 (let [suggestion-uid (gen-new-uid)
                                       open-page-uid  (<p! (get-open-page-uid))
                                       dgp-block-uid  (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                       dgp-discourse-graph-page-uid (:uid (get-child-with-str dgp-block-uid "Discourse graph this page"))]
                                   (println "suggestion uid" suggestion-uid "dgp-block-uid" dgp-block-uid "dgp-discourse-graph-page-uid" dgp-discourse-graph-page-uid)
                                   (println "pre-prompt" @pre-prompt)
                                   (if (not (some? @pre-prompt))
                                     (do
                                       (println "create bare struct")
                                       (-> (js/Promise.
                                             (fn [resolve _]
                                               (do
                                                (println "create bare struct")
                                                (create-bare-struct open-page-uid suggestion-uid)
                                                (resolve :done))))
                                           (.then (fn []
                                                   (println "get prompt")
                                                   (manual-prompt-guide dgp-discourse-graph-page-uid)))
                                           (.then (fn [prompt]
                                                    (reset! pre-prompt prompt)
                                                    (println "ask llm" @pre-prompt pre-prompt)
                                                    (ask-llm
                                                        block-uid
                                                        default-model
                                                        default-temp
                                                        default-max-tokens
                                                        get-linked-refs?
                                                        extract-query-pages?
                                                        extract-query-pages-ref?
                                                        active?
                                                        @pre-prompt
                                                        suggestion-uid
                                                        open-page-uid)))))
                                     (do
                                       (println "pre prompt exists")
                                       (create-bare-struct open-page-uid suggestion-uid)
                                       (ask-llm
                                         block-uid
                                         default-model
                                         default-temp
                                         default-max-tokens
                                         get-linked-refs?
                                         extract-query-pages?
                                         extract-query-pages-ref?
                                         active?
                                         @pre-prompt
                                         suggestion-uid
                                         open-page-uid))))))}
        "Discourse graph this page"]]]]))
