(ns ui.components.get-context
  (:require [cljs.core.async.interop :as asy :refer [<p!]]
    [reagent.core :as r :refer [atom]]
    [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]
    [ui.extract-data.chat :as ed :refer [extract-query-pages data-for-nodes get-all-images-for-node]]
    [ui.components.chat :refer [chat-context]]
    [ui.utils :refer [button-popover watch-string get-open-page-uid create-new-block button-with-tooltip model-mappings get-safety-settings update-block-string-for-block-with-child settings-button-popover image-to-text-for p get-child-of-child-with-str title->uid q block-has-child-with-str? call-llm-api update-block-string uid->title log get-child-with-str get-child-of-child-with-str-on-page get-open-page-uid get-block-parent-with-order get-focused-block create-struct gen-new-uid default-chat-struct get-todays-uid]]
    ["@blueprintjs/core" :as bp :refer [ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]))


;; visible pages as context
;; include query res
;; include dg refs
;; if under <800 tokens include linked refs

(defn get-context-button
  [messages-uid
   default-model
   default-temp
   default-max-tokens
   get-linked-refs?
   extract-query-pages?
   extract-query-pages-ref?
   active?
   pre-prompt
   remaining-prompt]

  (fn []
    (do
        [:div {:style {:flex "1 1 1 1"}}
         [button-with-tooltip
          "Summarize based on what is in the context window, also includes the linked discourse node references as context and
         if there is a query then we include all the context from the query result pages as well.
         "
          [:> Button
           {:minimal true
            :fill false
            :loading @active?
            :on-click (fn [e]
                        (when (not @active?)
                          (reset! active? true))
                        (go
                          (let [current-page-uid    (<p! (get-open-page-uid))
                                title               (uid->title current-page-uid)
                                tref                (if (nil? title)
                                                      (str "((" current-page-uid "))")
                                                      (str "[[" title "]]"))
                                nodes               {:children [{:string tref}]}
                                vision?            (= "gpt-4-vision" @default-model)
                                page-context-data   (extract-query-pages
                                                      {:context              nodes
                                                       :get-linked-refs?     @get-linked-refs?
                                                       :extract-query-pages? @extract-query-pages?
                                                       :only-pages?          @extract-query-pages-ref?
                                                       :vision?              vision?})
                                prompt              (str pre-prompt
                                                      "\n"
                                                      "<discourse-node-page-content> \n" page-context-data "\n </discourse-node-page-content> \n"
                                                      remaining-prompt)
                                context             (if vision?
                                                      (vec
                                                        (concat
                                                          [{:type "text"
                                                            :text (str pre-prompt "\n" remaining-prompt)}]
                                                          page-context-data))
                                                      prompt)
                                llm-context         [{:role "user"
                                                      :content context}]
                                settings           {:model        (get model-mappings @default-model)
                                                     :temperature @default-temp
                                                     :max-tokens  @default-max-tokens}
                                parent-block-uid   (gen-new-uid)
                                res-block-uid      (gen-new-uid)
                                already-context?    (block-has-child-with-str? current-page-uid "AI: Get context")
                                top-parent          (if (nil? already-context?)
                                                      current-page-uid
                                                      already-context?)
                                struct             {:s "AI: Get context"
                                                    :u parent-block-uid
                                                    :c [{:s ""
                                                         :u res-block-uid}]}]

                            (do
                              (if (some? messages-uid)
                                (create-new-block
                                  messages-uid
                                  "last"
                                  (str "**User:** ^^get context on:^^ " tref)
                                  #())
                                (create-struct struct top-parent res-block-uid false
                                  (p "Got new context")))
                             (call-llm-api
                               {:messages llm-context
                                :settings settings
                                :callback (fn [response]
                                            (let [res-str (-> response :body)]
                                              (if (some? messages-uid)
                                                (create-new-block
                                                  messages-uid
                                                  "last"
                                                  (str "**Assistant:** " res-str)
                                                  (js/setTimeout
                                                    (fn [] (reset! active? false))
                                                    500))
                                                (update-block-string
                                                  res-block-uid
                                                  (str res-str)
                                                  (js/setTimeout
                                                    (fn [] (reset! active? false))
                                                    500)))))})))))}
           "Get context"]]])))


(defn extract-context-children-data-as-str [context]
  (let [res (r/atom "")
        children (sort-by :order (:children @context))]
    (doseq [child children]
      (let [child-str (:string child)]
        ;(println "res " child-str)
        (swap! res str  (str child-str "\n"))))
    res))

(comment
  (-> (get-child-of-child-with-str-on-page
        "LLM chat settings" "Quick action buttons" "Get context" "Prompt")
    :children
    first
    :string)
  (sort-by :order (:children (get-child-of-child-with-str-on-page
                               "LLM chat settings" "Quick action buttons" "Get context" "Prompt")))

  (get-child-of-child-with-str (:uid (get-child-with-str
                                       (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                       "Get context")) "Prompt" "Pre-prompt"))



(defn get-suggestions-button [messages-uid
                              default-model
                              default-temp
                              default-max-tokens
                              get-linked-refs?
                              extract-query-pages?
                              extract-query-pages-ref?
                              active?
                              pre-prompt
                              remaining-prompt]
   (fn []
      [:div {:style {:flex "1 1 1 1"}}
       [button-with-tooltip
        "The LLM acts as a creative partner, providing ideas for next steps. It tries to follow our
           discourse graph workflow: ie if the context includes an experiment,
            try to identify observations (results) within the experiment page. if the context includes a result, try to
            tie it to a claim. if you have a claim, propose next possible experiments (issues) "
        [:> Button
         {:minimal true
          :fill false
          :loading @active?
          :on-click (fn [e]
                      (when (not @active?)
                        (reset! active? true))
                      (go
                        (let [current-page-uid    (<p! (get-open-page-uid))
                              title               (uid->title current-page-uid)
                              tref                (if (nil? title)
                                                    (str "((" current-page-uid "))")
                                                    (str "[[" title "]]"))
                              nodes               {:children [{:string tref}]}
                              vision?            (= "gpt-4-vision" @default-model)
                              page-context-data   (extract-query-pages
                                                    {:context              nodes
                                                     :get-linked-refs?     @get-linked-refs?
                                                     :extract-query-pages? @extract-query-pages?
                                                     :only-pages?          @extract-query-pages-ref?
                                                     :vision?              vision?})
                              prompt              (str pre-prompt
                                                    "\n"
                                                    "<discourse-node-page-content> \n" page-context-data "\n </discourse-node-page-content> \n"
                                                    remaining-prompt)
                              context             (if vision?
                                                     (vec
                                                       (concat
                                                         [{:type "text"
                                                           :text (str pre-prompt "\n" remaining-prompt)}]
                                                         page-context-data))
                                                     prompt)
                              llm-context         [{:role "user"
                                                    :content context}]
                              settings           {:model       (get model-mappings @default-model)
                                                  :temperature @default-temp
                                                  :max-tokens  @default-max-tokens}
                              parent-block-uid   (gen-new-uid)
                              res-block-uid      (gen-new-uid)
                              already-context?    (block-has-child-with-str? current-page-uid "AI: Get suggestions for next step ")
                              top-parent          (if (nil? already-context?)
                                                     current-page-uid
                                                     already-context?)
                              struct             {:s "AI: Get suggestions for next steps"
                                                  :u parent-block-uid
                                                  :c [{:s ""
                                                       :u res-block-uid}]}]
                          (do
                            (if (some? messages-uid)
                              (create-new-block
                                messages-uid
                                "last"
                                (str "**User:** ^^get suggestion on:^^ " tref)
                                #())
                              (create-struct struct top-parent res-block-uid false
                                (p "Got new suggestion")))
                           (call-llm-api
                             {:messages llm-context
                              :settings settings
                              :callback (fn [response]
                                          (let [res-str (-> response :body)]
                                            (if (some? messages-uid)
                                              (create-new-block
                                                messages-uid
                                                "last"
                                                (str "**Assistant:** " res-str)
                                                (js/setTimeout
                                                  (fn [] (reset! active? false))
                                                  500))
                                              (update-block-string
                                                res-block-uid
                                                (str res-str)
                                                (js/setTimeout
                                                  (fn [] (reset! active? false))
                                                  500)))))})))))}
         "Get Suggestions"]]]))
