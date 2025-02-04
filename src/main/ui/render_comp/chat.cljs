(ns ui.render-comp.chat
  (:require [reagent.core :as r]
            [applied-science.js-interop :as j]
            ["@blueprintjs/core" :as bp :refer [Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]
            [ui.components.chat :as comp :refer [chat-context chat-history]]
            [ui.components.chin :refer [chin]]
            [ui.utils :refer [extract-data pull-deep-block-data uid->eid get-safety-settings send-message-component model-mappings watch-children update-block-string-for-block-with-child watch-string create-struct settings-struct get-child-of-child-with-str q p get-parent-parent extract-from-code-block log update-block-string-and-move is-a-page? get-child-with-str move-block create-new-block]]
            [ui.actions.chat :refer [send-context-and-message load-context]]
            [reagent.dom :as rd]))



(defn chat-ui [block-uid]
  (js/console.time "chat-ui initial setup")
  (let [extracted-data (-> block-uid
                         (pull-deep-block-data)
                         extract-data)
        context             (r/atom (:context extracted-data))
        messages            (r/atom (:messages extracted-data))
        chat                (r/atom (:chat extracted-data))
        active?             (r/atom (:active? extracted-data))
        token-count         (r/atom (:token-count extracted-data))
        default-max-tokens  (r/atom (:max-tokens extracted-data))
        default-temp        (r/atom (:temperature extracted-data))
        default-model       (r/atom (:model extracted-data))
        get-linked-refs?    (r/atom (:get-linked-refs? extracted-data))
        extract-query-pages? (r/atom (:extract-query-pages? extracted-data))
        extract-query-pages-ref? (r/atom (:extract-query-pages-ref? extracted-data))
        popover-open?       (r/atom false)]
    (js/console.timeEnd "chat-ui initial setup")

    (js/console.time "chat-ui watch setup")
    (watch-children
      (:uid @messages)
      (fn [_ aft]
        (reset! messages aft)))

    (js/console.timeEnd "chat-ui watch setup")

    (fn [_]
     (js/console.time "chat-ui callback setup")
     (let [callback          (fn [{:keys [b-uid] :or {b-uid block-uid}}]
                               ;(p "called callback to load context")
                               (when (not @active?)
                                 (p "*Send* Button clicked")
                                 (do
                                   (update-block-string-for-block-with-child block-uid "Settings" "Active?" (str (not @active?)))
                                   (reset! active? true)
                                   (load-context
                                     {:chat-atom                chat
                                      :messages-atom            messages
                                      :parent-id                b-uid
                                      :active?                  active?
                                      :get-linked-refs?         get-linked-refs?
                                      :settings                 (merge
                                                                  {:model       (get model-mappings @default-model)
                                                                   :max-tokens  @default-max-tokens
                                                                   :temperature @default-temp}
                                                                  (when (= "gemini" @default-model)
                                                                    {:safety-settings (get-safety-settings b-uid)}))
                                      :token-count-atom         token-count
                                      :extract-query-pages?     extract-query-pages?
                                      :extract-query-pages-ref? extract-query-pages-ref?
                                      :vision?                  (= "gpt-4-vision" @default-model)}))))
           handle-key-event  (fn [event]
                               (when (and (.-altKey event) (= "Enter" (.-key event)))
                                 (let [buid (-> (j/call-in js/window [:roamAlphaAPI :ui :getFocusedBlock])
                                              (j/get :block-uid))
                                       b-parent (get-parent-parent buid)]
                                  (callback b-parent))))]
       (js/console.timeEnd "chat-ui callback setup")
       [:div
        {:class-name (str "chat-container-" block-uid)
         :on-click (fn []
                     (when @popover-open?
                       (reset! popover-open? false)))
         :style {:display "flex"
                 :flex-direction "column"
                 :border-radius "8px"
                 :overflow "hidden"}}
        [:> Card {:elevation 3
                  :style {:flex "1"
                          :margin "0"
                          :display "flex"
                          :flex-direction "column"
                          :border "2px solid rgba(0, 0, 0, 0.2)"
                          :border-radius "8px"}}

         (js/console.time "chat-ui top comp setup")
         [:div.top-comp
           {:class-name (str "chat-input-container-" block-uid)
            :style {:display "flex"
                    :flex-direction "row"
                    :box-shadow "rgb(100 100 100) 0px 0px 5px 0px"
                    :margin-bottom "15px"
                    :background-color "whitesmoke"
                    :border "1px"}}
           [chat-context context #() {:min-height     ""
                                      :background-color "whitesmoke"
                                      :padding-bottom "10px"}]]
         (js/console.timeEnd "chat-ui top comp setup")

         ;;  Using Keys to Force Re-renders: If msg-children is changing, but the chat-history'
         ;;  component isn't updating, try adding a key to the chat-history' component that changes whenever
         ;; 'msg-children changes. This could be a simple incremented counter or a hash of 'msg-children's]

         (js/console.time "chat-ui chat-history setup")
         [chat-history
          messages
          (:uid @messages)
          token-count
          default-model
          default-temp
          block-uid
          {:key (hash (:children @messages))}]
         (js/console.timeEnd "chat-ui chat-history setup")
         [:div.bottom-comp
          {:style {:box-shadow "rgb(175 104 230) 0px 0px 5px 0px"}}
          [:div.chat-input-container
           {:style {:display "flex"
                    :flex-direction "row"
                    :background-color "#f6cbfe3d"
                    :border "1px"}}
           (js/console.time "chat-ui chat-context comp setup")
           [chat-context chat handle-key-event]
           (js/console.timeEnd "chat-ui chat-context comp setup")]
          (js/console.time "chat-ui chin comp setup")
          [chin {:default-model        default-model
                 :default-max-tokens   default-max-tokens
                 :default-temp         default-temp
                 :get-linked-refs?      get-linked-refs?
                 :active?              active?
                 :block-uid            block-uid
                 :callback             callback
                 :extract-query-pages? extract-query-pages?
                 :extract-query-pages-ref? extract-query-pages-ref?
                 :popover-open?        popover-open?}]
          (js/console.timeEnd "chat-ui chin comp setup")]]]))))


(defn main [{:keys [:block-uid]} & args]
  (let [parent-el (.getElementById js/document (str (second args)))]
    (.addEventListener parent-el "mousedown" (fn [e] (.stopPropagation e)))
    (rd/render [chat-ui block-uid] parent-el)))
