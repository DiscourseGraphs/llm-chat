(ns ui.actions.dg-this-page
  (:require [cljs.core.async.interop :as asy :refer [<p!]]
            [reagent.core :as r :refer [atom]]
            [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]
            [ui.extract-data.chat :as ed :refer [extract-query-pages data-for-nodes get-all-images-for-node]]
            [ui.components.chat :refer [chat-context]]
            [ui.components.chin :refer [chin]]
            [ui.utils :refer [button-popover button-with-tooltip watch-string model-mappings get-safety-settings update-block-string-for-block-with-child settings-button-popover image-to-text-for p get-child-of-child-with-str title->uid q block-has-child-with-str? call-llm-api update-block-string uid->title log get-child-with-str get-child-of-child-with-str-on-page get-open-page-uid get-block-parent-with-order get-focused-block create-struct gen-new-uid default-chat-struct get-todays-uid]]
            ["@blueprintjs/core" :as bp :refer [ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]))

(defn extract-context-children-data-as-str [context]
  (let [res (r/atom "")
        children (sort-by :order (:children @context))]
    (doseq [child children]
      (let [child-str (:string child)]
        (swap! res str  (str child-str "\n"))))
    res))

(defn create-bare-struct [open-page-uid suggestion-uid loading-messages-uid default-msg]
  (go
   (let [already-suggested? (block-has-child-with-str? open-page-uid  "AI: Discourse node suggestions")
         suggestion-comp-uid (gen-new-uid)
         struct             (if (nil? already-suggested?)
                              {:s "AI: Discourse node suggestions"
                               :op false
                               :c [{:s "{{llm-dg-suggestions}}"
                                    :u suggestion-comp-uid
                                    :op false
                                    :c [{:s "Suggestions"
                                         :u suggestion-uid}
                                        {:s "Loading messages"
                                         :c [{:s default-msg
                                              :u loading-messages-uid}]}]}]}
                              {:s "{{llm-dg-suggestions}}"
                               :op false
                               :u suggestion-comp-uid
                               :c [{:s "Suggestions"
                                    :u suggestion-uid}
                                   {:s "Loading messages"
                                    :c [{:s default-msg
                                         :u loading-messages-uid}]}]})

         top-parent          (if (nil? already-suggested?)
                               open-page-uid
                               already-suggested?)]
     (p "already-suggested? " already-suggested? open-page-uid)
     (create-struct
       struct
       top-parent
       suggestion-comp-uid
       true))))



(defn ask-llm [block-uid
               default-model
               default-temp
               default-max-tokens
               get-linked-refs?
               extract-query-pages?
               extract-query-pages-ref?
               active?
               pre-prompt
               suggestion-uid
               open-page-uid]
    (let [pre                "*Discourse graph this page* "
          title              (uid->title open-page-uid)
          nodes              (if (nil? title)
                               {:children [{:string (str "((" open-page-uid "))")}]}
                               {:children [{:string (str "[[" title "]]" "\n")}]})
          vision?            (= "gpt-4-vision" @default-model)
          extracted-qry-pg   (extract-query-pages
                               {:context              nodes
                                :get-linked-refs?     @get-linked-refs?
                                :extract-query-pages? @extract-query-pages?
                                :only-pages?          @extract-query-pages-ref?
                                :vision?              vision?})
          content            (if vision?
                               (vec
                                 (concat
                                   [{:type "text"
                                     :text (str pre-prompt)}]
                                   extracted-qry-pg))
                               (clojure.string/join
                                 "\n"
                                 [(str pre-prompt)
                                  extracted-qry-pg]))
          messages           [{:role "user"
                               :content content}]
          settings           (merge
                               {:model       (get model-mappings @default-model)
                                :temperature @default-temp
                                :max-tokens  @default-max-tokens}
                               (when (= "gemini" @default-model)
                                 {:safety-settings (get-safety-settings block-uid)}))]
        (do
          (p (str pre "Calling openai api, with settings : " settings))
          (p (str pre "and messages : " messages))
          (p "context""\n ******************** \n" pre-prompt)
          (p (str pre "Now sending message and wait for response ....."))
          (call-llm-api
            {:messages messages
             :settings settings
             :callback (fn [response]
                         (p (str pre "llm response received: " response))
                         (let [res-str             (map
                                                     (fn [s]
                                                       (when (not-empty s)
                                                         {:s (str s)}))
                                                     (-> response
                                                       :body
                                                       clojure.string/split-lines))]
                           (p "suggestions: " res-str)
                           (do
                             (create-struct
                               {:u suggestion-uid
                                :c (vec res-str)}
                               suggestion-uid
                               nil
                               false
                               (js/setTimeout
                                 (fn []
                                   (p (str pre "Updated block " suggestion-uid " with suggestions from openai api"))
                                   (reset! active? false))
                                 500)))))}))))