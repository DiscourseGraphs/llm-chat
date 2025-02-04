(ns ui.components.chin
  (:require [reagent.core :as r]
            [applied-science.js-interop :as j]
            [ui.components.get-context :refer [get-context-button get-suggestions-button]]
            [ui.utils :refer [extract-data pull-deep-block-data block-has-child-with-str? title->uid settings-button-popover buttons-settings gemini-safety-settings-struct button-with-tooltip get-child-of-child-with-str create-struct button-popover p get-child-with-str watch-children update-block-string-for-block-with-child]]
            ["@blueprintjs/core" :as bp :refer [RadioGroup MenuDivider Position Radio ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]))

(defn inject-style []
  (let [style-element (.createElement js/document "style")
        css-string ".sp svg { color: cadetblue; }"] ; Change 'blue' to your desired color

    (set! (.-type style-element) "text/css")
    (when (.-styleSheet style-element) ; For IE8 and below.
      (set! (.-cssText (.-styleSheet style-element)) css-string))
    (when-not (.-styleSheet style-element) ; For modern browsers.
      (let [text-node (.createTextNode js/document css-string)]
        (.appendChild style-element text-node)))
    (.appendChild (.-head js/document) style-element)))

(defn send-message-component [active? callback]
  (inject-style)
  [button-with-tooltip
   "Send your message."
   [:> Button {:class-name "sp"
               :style {:width "30px"}
               :icon (if @active? nil "send-message")
               :minimal true
               :fill false
               :small true
               :loading @active?
               :on-click #(callback {})}]])


(defn gemini-safety-component [block-uid]
  (let [settings-uid      (:uid (get-child-with-str block-uid "Settings"))
        harassment        (r/atom (get-child-of-child-with-str settings-uid "Safety settings" "Harassment"))
        hate-speech       (r/atom (get-child-of-child-with-str settings-uid "Safety settings" "Hate Speech"))
        sexually-explicit (r/atom (get-child-of-child-with-str settings-uid "Safety settings" "Sexually Explicit"))
        dangerous-content (r/atom (get-child-of-child-with-str settings-uid "Safety settings" "Dangerous Content"))]
    (fn [_]
      [button-popover
       "Safety settings"
       [:div ;.bp3-popover-dismiss
        [:> RadioGroup
         {:label "Harassment"
          :inline true
          :on-change (fn [v]
                       (let [e (.-value (.-currentTarget v))]
                        (update-block-string-for-block-with-child settings-uid "Safety settings" "Harassment" e)
                        (reset! harassment e)))
          :selected-value @harassment}
         [:> Radio {:label "Block none" :value "Block none"}]
         [:> Radio {:label "Block few" :value "Block few"}]
         [:> Radio {:label "Block some" :value "Block some"}]
         [:> Radio {:label "Block most" :value "Block most"}]]
        [:> RadioGroup
         {:label "Hate Speech"
          :inline true
          :on-change (fn [v]
                       (let [e (.-value (.-currentTarget v))]
                        (update-block-string-for-block-with-child settings-uid "Safety settings" "Hate Speech" e)
                        (reset! hate-speech e)))
          :selected-value @hate-speech}
         [:> Radio {:label "Block none" :value "Block none"}]
         [:> Radio {:label "Block few" :value "Block few"}]
         [:> Radio {:label "Block some" :value "Block some"}]
         [:> Radio {:label "Block most" :value "Block most"}]]
        [:> RadioGroup
         {:label "Sexually Explicit"
          :inline true
          :on-change (fn [v]
                       (let [e (.-value (.-currentTarget v))]
                         (update-block-string-for-block-with-child settings-uid "Safety settings" "Sexually Explicit" e)
                         (reset! sexually-explicit e)))
          :selected-value @sexually-explicit}
         [:> Radio {:label "Block none" :value "Block none"}]
         [:> Radio {:label "Block few" :value "Block few"}]
         [:> Radio {:label "Block some" :value "Block some"}]
         [:> Radio {:label "Block most" :value "Block most"}]]
        [:> RadioGroup
         {:label "Dangerous Content"
          :inline true
          :on-change (fn [v]
                       (let [e (.-value (.-currentTarget v))]
                        (update-block-string-for-block-with-child settings-uid "Safety settings" "Dangerous Content" e)
                        (reset! dangerous-content e)))
          :selected-value @dangerous-content}
         [:> Radio {:label "Block none" :value "Block none"}]
         [:> Radio {:label "Block few" :value "Block few"}]
         [:> Radio {:label "Block some" :value "Block some"}]
         [:> Radio {:label "Block most" :value "Block most"}]]]])))


(defn chin [{:keys [popover-open? default-model default-max-tokens default-temp get-linked-refs? active? block-uid callback buttons? extract-query-pages? extract-query-pages-ref?]}]
  (let [get-context-data          (-> (:uid (get-child-with-str
                                              (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                              "Get context"))
                                    (pull-deep-block-data)
                                    extract-data)
        co-default-temp             (r/atom (:temperature get-context-data))
        co-default-model            (r/atom (:model get-context-data))
        co-default-max-tokens       (r/atom (:max-tokens get-context-data))
        co-get-linked-refs?         (r/atom (:get-linked-refs? get-context-data))
        co-extract-query-pages?     (r/atom (:extract-query-pages? get-context-data))
        co-extract-query-pages-ref? (r/atom (:extract-query-pages-ref? get-context-data))
        co-pre-prompt               (:pre-prompt get-context-data)
        co-remaining-prompt         (:further-instructions get-context-data)
        co-active?                  (r/atom (:active? get-context-data))

        get-suggestions-data        (-> (:uid (get-child-with-str
                                               (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                               "Get suggestions"))
                                      (pull-deep-block-data)
                                      extract-data)
        sug-default-model            (r/atom (:model get-suggestions-data))
        sug-default-temp             (r/atom (:temperature get-suggestions-data))
        sug-default-max-tokens       (r/atom (:max-tokens get-suggestions-data))
        sug-get-linked-refs?         (r/atom (:get-linked-refs? get-suggestions-data))
        sug-extract-query-pages?     (r/atom (:extract-query-pages? get-suggestions-data))
        sug-extract-query-pages-ref? (r/atom (:extract-query-pages-ref? get-suggestions-data))
        sug-pre-prompt               (:pre-prompt get-suggestions-data)
        sug-remaining-prompt         (:further-instructions get-suggestions-data)
        sug-active?                  (r/atom (:active? get-suggestions-data))
        messages-uid                  (:uid (get-child-with-str block-uid "Messages"))]
    (fn [_]
      [:div.chin
       {:style {:display "flex"
                :flex-direction "row"
                :background-color "#f6cbfe3d"
                :min-height "27px"
                :justify-content "space-between"
                :font-size "10px"
                :padding-right "11px"
                :align-items "center"
                :border "1px"}}
       [:> ButtonGroup
        [get-context-button
         messages-uid
         co-default-model
         co-default-temp
         co-default-max-tokens
         co-get-linked-refs?
         co-extract-query-pages?
         co-extract-query-pages-ref?
         co-active?
         co-pre-prompt
         co-remaining-prompt]
        [get-suggestions-button
         messages-uid
         sug-default-model
         sug-default-temp
         sug-default-max-tokens
         sug-get-linked-refs?
         sug-extract-query-pages?
         sug-extract-query-pages-ref?
         sug-active?
         sug-pre-prompt
         sug-remaining-prompt]]
       [:> ButtonGroup
         [:> Popover
          {:is-open @popover-open?
           :onInteraction (fn [next-open-state]
                            (reset! popover-open? next-open-state))
           :usePortal true}
          [:> Button {:icon "cog"
                      :minimal true
                      :aria-hidden nil
                      :tabIndex -1
                      :small true
                      :on-click (fn [e]
                                  (.stopPropagation e)
                                  (reset! popover-open? (not @popover-open?)))
                      :style {:background-color "#fdf3ff"}}]
          [:> Menu
           {:style {:padding "20px"}
            :on-click #(.stopPropagation %)}
           [buttons-settings
            "Chat settings"
            block-uid
            default-temp
            default-model
            default-max-tokens
            get-linked-refs?
            extract-query-pages?
            extract-query-pages-ref?]]]
        [send-message-component
         active?
         callback]]])))







    
