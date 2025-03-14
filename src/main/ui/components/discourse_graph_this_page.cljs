(ns ui.components.discourse-graph-this-page
  (:require [cljs.core.async.interop :as asy :refer [<p!]]
            [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]
            [clojure.string :as str]
            [ui.actions.dg-this-page :refer [create-bare-struct get-llm-response ask-llm prepare-prompt-with-plain-context]]
            [ui.utils :refer [extract-from-code-block uid->eid button-with-tooltip p title->uid q block-has-child-with-str? get-child-with-str get-child-of-child-with-str-on-page get-open-page-uid create-struct gen-new-uid]]
            [ui.extract-data.create-prompts :refer [manual-prompt-guide]]
            ["@blueprintjs/core" :as bp :refer [Button ButtonGroup]]))


(defn add-response-to-block [block-uid children active?]
      (create-struct
          {:u block-uid
           :c children}
          block-uid
          nil
          false
          (js/setTimeout
            (fn []
              (p (str "Updated block " block-uid " with suggestions from openai api")))
              ;(reset! active? false))
            500)))


(defn get-refs-for-all-suggestions [open-page-uid suggestion-uid model-settings active? prompt]
  (println "13 get-refs-for-all-suggestions" open-page-uid suggestion-uid)
  (let [suggestions-eid  (uid->eid suggestion-uid)
        all-suggestions  (:children (ffirst (q '[:find (pull ?eid [{:block/children ...}
                                                                   :block/string :block/uid])
                                                 :in $ ?eid]
                                              suggestions-eid)))
        raw-context-data (ffirst (q '[:find (pull ?eid [{:block/children ...} :block/string :block/order :block/uid])
                                      :in $ ?eid]
                                   (uid->eid open-page-uid)))
        ctr (atom 0)]
    (p "15 all-suggestions" (count all-suggestions))
    (go
     (doseq [suggestion all-suggestions]
       (do
          (p "16 suggestion" suggestion)
         (let [prepared-data (str prompt
                               "\n <raw-data> \n "
                               raw-context-data
                               "\n <\raw-data> \n <discourse-node-suggestion> "
                               (:string suggestion)
                               "\n </discourse-node-suggestion>")
               llm-response  (<! (get-llm-response prepared-data suggestion-uid model-settings))
               extracted (js->clj (js/JSON.parse (-> llm-response
                                                   :body
                                                   (str/replace #"```json\s*" "") ; Remove ```json
                                                   (str/replace #"```\s*$" "")    ; Remove trailing ```
                                                   str/trim
                                                   extract-from-code-block))
                           :keywordize-keys true)
               children (map (fn [s]
                                 {:s (str "\n ((" (:uid s) ")) \n")})
                             extracted)]
           (println "CHILDREN" children)

           (<! (add-response-to-block (:uid suggestion) children active?))))))))

(defn get-suggestions-from-llm [model-settings block-uid suggestion-uid active? pre-prompt]
  (go
   (let [prepared-data (<! (prepare-prompt-with-plain-context pre-prompt model-settings))
         _ (p "prepared-data" prepared-data)
         llm-response (<! (get-llm-response prepared-data block-uid model-settings))
         _ (p "llm-response" llm-response)
         children  (map
                     (fn [s]
                       (when (not-empty s)
                         {:s (str s)}))
                     (-> llm-response
                      :body
                      clojure.string/split-lines))]
     (<! (add-response-to-block suggestion-uid children active?)))))


(comment 
  ;; from results graph
  (get-refs-for-all "4FIdGNwsK" ""))


(defn create-ref-relevent-prompt []
  (let [prompt "Extract the relevant notes for a specific Discourse-node-suggestion from the provided hierarchical roam graph data.\nDiscourse graphs are an information model for bodies of knowledge that emphasize discourse moves (such as questions, claims, and evidence), and relations (such as support or opposition), rather than papers or sources as the main units.\nYour objective is to extract blocks containing the `uid` and `string` fields based on how relevant they are to discourse node suggestion. Ensure the extracted blocks follow proper rules to handle the hierarchical structure as detailed below.\n\n<Task-Details>\n\n- You will be given:\n  - <Raw-data> - A hierarchical tree structure representing a Roam Research graph.\n  - <Discourse-node-suggestion>\n\n- Follow these extraction rules:\n  1. **Rule 1:** If *all* blocks within a parent node are relevant to the suggestion, retrieve only the parent's `uid`.\n  2. **Rule 2:** If *some but not all* blocks of a parent node are relevant to the suggestion, retrieve the `uid` for each matching block individually.\n</Task-Details>\n\n<Understanding-the-Data-Structure>\n\n- **Nodes**:\n  - Each node is raw representation of a roam research block that may contain:\n    - `:string`: Text content of the node.\n    - `:uid`: A unique identifier for the node.\n    - `:order`: An integer indicating the node's sequence relative to its siblings.\n    - `:children` (optional): A list of child nodes, each following the same structure as the parent.\n\n- **Hierarchy**:\n  - Nodes may have nested `:children`, forming a hierarchical tree structure.\n  - Nesting reflects indentation levels and parent-child relationships.\n</Understanding-the-Data-Structure>\n\n<Requirements>\n\n- Extract and show only the `uid` of the relevant blocks .\n- Apply the rules outlined regarding hierarchy and specificity.\n- **For Rule 1**, if all the children of a parent are relevant, include only the parent's `uid` (effectively embedding all its children in roam research speak).\n- **For Rule 2**, if only some children are relevant, extract each relevant child node individually, maintaining the specificity of the extraction.\n- Ensure that the relationships between nodes are properly considered when selecting nodes.\n- Do not provide additional commentary or unrelated blocks. ONLY the uid of relevant blocks.\n</Requirements>\n\n<Output-Format>\n\nThe output should present a list-of-uid JSON format. Each entry/element in the list must contain  ONLY the following fields:\n- `\"uid\"`: Corresponding to the `uid` of the extracted block.\n\n<Important-note> ONLY REPLY in the JSON format described below, do not give ANY sort of context, you have to stick to the the Output-Format see the example-output below. </Important-note>\n<Output-Format>\n\n<Example-Output>\n[\n  {\"uid\": \"JdAOhRycm\"},\n  {\"uid\": \"Dh55R-KAr\"}\n]\n  </Example-Output>\n\n<Notes>\n- **Important Distinction**: Rule 1 is particularly nuanced in handling parent-child relationships. If all child nodes are mentioned in the summary, include *only* the parent's `uid` and `string`. This is akin to embedding the entire structure in one reference.\n- When extracting nodes hierarchically, make sure to apply Rules 1 and 2 precisely.\n- Do **not** include any blocks that are irrelevant or not mentioned in the provided summary.\n- Ensure proper evaluation of parent-child relationships to guarantee correctness in extraction, particularly regarding whether to embed (Rule 1) or reference each child separately (Rule 2).\n</Notes>\n"
        uid   (:uid (get-child-of-child-with-str-on-page  "LLM chat settings" "Quick action buttons" "Discourse graph this page" "Prompt"))]
    (create-struct
      {:u uid
       :c [{:s "Ref relevant notes prompt"
            :c [{:s prompt}]}]}
      uid
      nil
      false
      ())))


(defn discourse-graph-this-page-button [block-uid
                                        default-model
                                        default-temp
                                        default-max-tokens
                                        get-linked-refs?
                                        extract-query-pages?
                                        extract-query-pages-ref?
                                        active?
                                        context
                                        pre-prompt
                                        ref-relevant-prompt]
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
                                   (when (not (some? @ref-relevant-prompt))
                                     (create-ref-relevent-prompt))
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
                                               open-page-uid
                                               suggestion-uid
                                               model-settings
                                               active?
                                               ref-relevant-prompt)))
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
                                             open-page-uid
                                             suggestion-uid
                                             model-settings
                                             active?
                                             @ref-relevant-prompt)))))))}
        "Discourse graph this page"]]]]))
