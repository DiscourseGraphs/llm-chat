(ns ui.components.discourse-graph-this-page
  (:require [cljs.core.async.interop :as asy :refer [<p!]]
            [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]
            [ui.actions.dg-this-page :refer [create-bare-struct get-llm-response ask-llm prepare-prompt-with-plain-context]]
            [ui.utils :refer [uid->eid button-with-tooltip p title->uid q block-has-child-with-str? get-child-with-str get-child-of-child-with-str-on-page get-open-page-uid create-struct gen-new-uid]]
            [ui.extract-data.create-prompts :refer [manual-prompt-guide]]
            ["@blueprintjs/core" :as bp :refer [Button ButtonGroup]]))


(defn add-response-to-block [response block-uid active?]
    (let [res-str (map
                    (fn [s]
                      (when (not-empty s)
                        {:s (str s)}))
                    (-> response
                        :body
                        clojure.string/split-lines))]
      (create-struct
          {:u block-uid
           :c (vec res-str)}
          block-uid
          nil
          false
          (js/setTimeout
            (fn []
              (p (str "Updated block " block-uid " with suggestions from openai api")))
              ;(reset! active? false))
            500))))

(defn get-refs-for-all-suggestions [suggestion-uid model-settings active?]
  (let [suggestions-eid  (uid->eid suggestion-uid)
        all-suggestions  (:children (ffirst (q '[:find (pull ?eid [{:block/children ...}
                                                                   :block/string :block/uid])
                                                 :in $ ?eid]
                                              suggestions-eid)))
        raw-context-data (ffirst (q '[:find (pull ?eid [{:block/children ...} :block/string :block/order :block/uid])
                                      :in $ ?eid]
                                   suggestions-eid))
        prompt (get-child-of-child-with-str-on-page "LLM chat settings" "Ref relevant notes" "Prompt" "Step-2")]
    (p "15 all-suggestions" (count all-suggestions))
    (go
     (doseq [suggestion all-suggestions]
       (do
          (p "16 suggestion" suggestion)
         (let [prepared-data (str prompt "\n Raw data: " raw-context-data "\n Suggestion: " (:string suggestion))
               llm-response  (<! (get-llm-response prepared-data suggestion-uid model-settings))]
           (<! (add-response-to-block llm-response (:uid suggestion) active?))))))))

(defn get-suggestions-from-llm [model-settings block-uid suggestion-uid active? pre-prompt]
  (go
   (let [prepared-data (<! (prepare-prompt-with-plain-context pre-prompt model-settings))
         _ (p "prepared-data" prepared-data)
         llm-response (<! (get-llm-response prepared-data block-uid model-settings))
         _ (p "llm-response" llm-response)]
     (<! (add-response-to-block llm-response suggestion-uid active?)))))


(comment 
  ;; from results graph
  (get-refs-for-all "4FIdGNwsK" ""))
      

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
    #_(p "--" (get-child-of-child-with-str-on-page "llm chat" "Quick action buttons" button-name "Context"))
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
                               (p "clicked discourse graph this page")
                               (when (not @active?)
                                 (reset! active? true))
                               (go
                                 (let [suggestion-uid (gen-new-uid)
                                       open-page-uid  (<p! (get-open-page-uid))
                                       loading-message-uid (gen-new-uid)
                                       dgp-block-uid  (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                       dgp-discourse-graph-page-uid (:uid (get-child-with-str dgp-block-uid "Discourse graph this page"))
                                       model-settings  {:model default-model
                                                        :temperature default-temp
                                                        :max-tokens default-max-tokens
                                                        :get-linked-refs? get-linked-refs?
                                                        :extract-query-pages? extract-query-pages?
                                                        :extract-query-pages-ref? extract-query-pages-ref?}]
                                   (p "1 suggestion uid" suggestion-uid "dgp-block-uid" dgp-block-uid "dgp-discourse-graph-page-uid" dgp-discourse-graph-page-uid)
                                   (p "2 pre-prompt" (some? @pre-prompt))
                                   (if (not (some? @pre-prompt))
                                     (do
                                       (<! (create-bare-struct open-page-uid suggestion-uid loading-message-uid
                                            "Setting this up: This graph does not have a pre-prompt yet, setting up the prompt now..."))
                                       (reset! pre-prompt (<! (manual-prompt-guide dgp-discourse-graph-page-uid loading-message-uid)))
                                       (<! (get-suggestions-from-llm
                                               model-settings
                                               block-uid
                                               suggestion-uid
                                               active?
                                               @pre-prompt))
                                       (<! (get-refs-for-all-suggestions
                                               suggestion-uid
                                               model-settings
                                               active?)))
                                     (do
                                       (p "3 pre prompt exists" pre-prompt)
                                       (<! (create-bare-struct
                                             open-page-uid
                                             suggestion-uid
                                             loading-message-uid
                                             "8 Asking llm please wait..."))
                                       (<! (get-suggestions-from-llm
                                             model-settings
                                             block-uid
                                             suggestion-uid
                                             active?
                                             @pre-prompt))
                                       (p "14 get refs for all")
                                       (<! (get-refs-for-all-suggestions
                                             suggestion-uid
                                             model-settings
                                             active?)))))))}
        "Discourse graph this page"]]]]))
