(ns ui.components.chat
  (:require [reagent.core :as r]
            [applied-science.js-interop :as j]
            [ui.utils :refer [p button-with-tooltip get-child-of-child-with-str-on-page model-mappings get-safety-settings create-alternate-messages call-llm-api create-struct gen-new-uid delete-block get-child-with-str watch-children update-block-string-for-block-with-child]]
            ["@blueprintjs/core" :as bp :refer [ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]))

(defn log
  [& args]  (apply js/console.log args))



(defn chat-context
  ([context handle-keydown-event]
   (chat-context context handle-keydown-event {}))
  ([context handle-keydown-event style-map]
   (fn [_ _ _]
     (let [uid (:uid @context)]
       [:div
        {:class-name (str (if (not-empty style-map)
                            "chat-area-"
                            "chat-context-") uid)
         :ref (fn [el]
                (when (and (some? el)
                        uid)
                 (-> (j/call-in js/window [:roamAlphaAPI :ui :components :renderBlock]
                       (clj->js {:uid       uid
                                 :zoom-path true
                                 :el        el}))
                   (.then
                     (fn [_]
                       (p "chat context block rendered successfully")))
                   (.catch (fn [e]
                             (log "Error in" (if (not-empty style-map)
                                               "chat-area-"
                                               "chat-context-") uid "--" e "--" "--" el))))))
         :on-key-down handle-keydown-event
         :style (merge {:flex "1 1 auto"
                        :height "100%"
                        :overflow "auto"
                        :flex-direction "column"
                        :display "flex"
                        :align-items "stretch"
                        :background-color "#f6cbfe3d"
                        :min-height "100px"
                        :max-height "700px"}
                  style-map)}]))))

(defn child-node [child]
  (fn [_]
    (let [uid (:uid child)]
      [:div
       {:class-name (str "child-node-" uid)
        :style {:display "flex"}}
       [:div
        {:class-name (str "child-node-render-")
         :style {:flex "1"}
         :ref (fn [el]
                (when (some? el)
                  (-> (j/call-in js/window [:roamAlphaAPI :ui :components :renderBlock]
                        (clj->js {:uid       uid
                                  :open      false
                                  :zoom-path false
                                  :el        el}))
                    (.then
                      (fn [_]
                        (p "MESSAGES block rendered successfully"))))))}]])))

(defn chat-history [m-children m-uid token-count model temp block-uid]
  (let [history-ref (atom nil)
        active? (r/atom false)]

    (fn [_ _ _]
      (let [tc       @token-count
            children (vec (sort-by :order (:children @m-children)))
            temp     @temp
            model    @model
            suggestion-context (str (-> (get-child-of-child-with-str-on-page "LLM chat settings" "Quick action buttons" "Settings" "Context for discourse graph suggestion")
                                      :children
                                      first
                                      :string))]
        (js/console.time "chat-ui detailed-chat-history setup")
        ;(p "TOKEN COUNT" tc)
        [:div.middle-comp
         {:class-name (str "chat-history-container-" m-uid)
          :style {:display "flex"
                  :box-shadow "#2e4ba4d1 0px 0px 4px 0px"
                  :margin-bottom "15px"
                  :flex-direction "column"}}
         [:div
          {:class-name (str "chat-history-" m-uid)
           :ref (fn [el]
                  (when (some? el)
                    (do
                      (set! (.-scrollTop el) (.-scrollHeight el))
                      (reset! history-ref el))))
           :style {:flex "1"
                   :overflow-y "auto"
                   :min-height "300px"
                   :max-height "700px"
                   :background "aliceblue"}}
          (doall
           (for [child children]
             ^{:key (:uid child)}
             [child-node child]))]
         [:div
          {:class-name (str "messages-chin-" m-uid)
           :style {:display "flex"
                   :flex-direction "row"
                   :background-color "aliceblue"
                   :min-height "27px"
                   :justify-content "end"
                   :font-size "10px"
                   :padding "8px"
                   :align-items "center"
                   :border "1px"}}
          [button-with-tooltip
           "Total number of tokens used in this chat till now, including the data extracted from context pages."
           [:span (str "Tokens used: " tc)]]
          [button-with-tooltip
           "Scroll to the bottom of messages, i.e show the latest chat messages."
           [:> Button {:class-name (str "scroll-down-button" m-uid)
                       :style {:width "30px"}
                       :icon "chevron-down"
                       :minimal true
                       :fill false
                       :small true
                       :on-click #(let [el @history-ref]
                                    (p "scroll down button clicked")
                                    (when el
                                      (set! (.-scrollTop el) (.-scrollHeight el))))}]]
          [button-with-tooltip
           "Scroll to the top of messages, i.e show the oldest chat messages."
           [:> Button {:class-name (str "scroll-up-button" m-uid)
                       :style {:width "30px"}
                       :icon "chevron-up"
                       :minimal true
                       :fill false
                       :small true
                       :on-click #(let [el @history-ref]
                                    (p "scroll up button clicked")
                                    (when el
                                      (set! (.-scrollTop el) 0)))}]]
          (js/console.timeEnd "chat-ui detailed-chat-history setup")
          [button-with-tooltip
           "Based on the chat conversation till now, the llm will propose a list of discourse nodes. You can then
            select the ones you want and discard others."
           [:> Button {:class-name (str "llm-dg-suggestions-button" m-uid)
                       :style {:width "30px"}
                       :icon "new-link"
                       :minimal true
                       :fill false
                       :loading @active?
                       :small true
                       :on-click (fn [_]
                                   (when (not @active?)
                                     (reset! active? true)
                                     (let [pre                "*Chat history suggestions: * "
                                           suggestion-uid     (gen-new-uid)
                                           struct             {:s "{{llm-dg-suggestions}}"
                                                                 :op false
                                                                 :c [{:s "Suggestions"
                                                                      :u suggestion-uid}]}
                                           filtered-messages  (filterv
                                                                (fn [n]
                                                                  (not= "{{llm-dg-suggestions}}" (:string n)))
                                                                (sort-by :order (:children (get-child-with-str block-uid "Messages"))))
                                           settings           (merge
                                                                {:model       (get model-mappings model)
                                                                 :temperature temp
                                                                 :max-tokens  500}
                                                                (when (= "gemini" model)
                                                                  {:safety-settings (get-safety-settings block-uid)}))
                                           messages           (vec
                                                                (conj
                                                                  (create-alternate-messages filtered-messages "_" pre)
                                                                  {:role "user"
                                                                   :content suggestion-context}))]
                                       (do
                                         (create-struct
                                           struct
                                           m-uid
                                           nil
                                           false)
                                         (p (str pre "Calling openai api, with settings : " settings))
                                         (p (str pre "and messages : " messages))
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
                                                                500)))))})))))}]]]]))))


