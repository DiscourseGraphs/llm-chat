(ns ui.components.bottom-bar
  (:require [cljs.core.async.interop :as asy :refer [<p!]]
            [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]
            [reagent.core :as r :refer [atom]]
            [ui.components.quick-buttons :refer [ discourse-graph-this-page-button]]
            [cljs-http.client :as http]
            [ui.components.get-context :refer [get-context-button get-suggestions-button]]
            [ui.components.search-pinecone :refer [search-pinecone]]
            [ui.extract-data.chat :refer [data-for-nodes get-all-images-for-node]]
            [clojure.string :as str]
            [ui.components.graph-overview-ai :refer [filtered-pages-button]]
            [ui.extract-data.chat :refer [extract-query-pages get-all-images-for-node]]
            [applied-science.js-interop :as j]
            [ui.utils :refer [pull-deep-block-data extract-data get-all-users buttons-settings extract-from-code-block create-new-block uid->eid get-current-user chat-ui-with-context-struct ai-block-exists? button-popover button-with-tooltip model-mappings get-safety-settings update-block-string-for-block-with-child settings-button-popover image-to-text-for p get-child-of-child-with-str title->uid q block-has-child-with-str? call-llm-api update-block-string uid->title log get-child-with-str get-child-of-child-with-str-on-page get-open-page-uid get-block-parent-with-order get-focused-block create-struct gen-new-uid default-chat-struct get-todays-uid]]
            ["@blueprintjs/core" :as bp :refer [ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]))



(defn bottom-bar-gear []
  [:> ButtonGroup
   {:class-name "button-with-settings"
    :style {:overflow "hidden"
            :display "flex"
            :flex-direction "row"
            :justify-content "space-between"
            :align-items "center"
            :flex "1 1 1"}
    :minimal true}
   [:div {:class-name "Classes.POPOVER_DISMISS_OVERRIDE"
          :style {:flex "1 1 1"}}
    [:> Popover
      [:> Button {:icon "cog"
                      :minimal true
                      :small true
                      :style {:background-color "#eeebeb"}}]
     [:> Menu
      {:style {:padding "20px"}
       :class-name "Classes.POPOVER_DISMISS_OVERRIDE"}
      (doall
        (for [button ["Discourse graph this page" "Get context" "Get suggestions"]]
           ^{:key button}
           [buttons-settings button]))]]]])

(def db->template-username
  {"Matt Akamatsu"       "Matt"
   "Valerie Bentivegna"   "Valerie"
   "Maggie Fuqua"         "Maggie"
   "Hanna Bekele"         "Hanna"
   "Atsushi Matsuda"      "Atsushi"
   "Emma Koves"           "Emma"
   "Aadarsh Raghunathan"  "Aadarsh"
   "Benjamin Brown"       "Benjamin"
   "Abhishek Raghunathan" "Abhi"
   "sid"                  "Sid"})

(def db->meetings-username
  {"Matt Akamatsu"       "Matt"
   "Valerie Bentivegna"   "Valerie"
   "Maggie Fuqua"         "Maggie"
   "Hanna Bekele"         "Hanna Bekele"
   "Atsushi Matsuda"      "Atsushi"
   "Emma Koves"           "Emma"
   "Aadarsh Raghunathan"  "Aadarsh"
   "Benjamin Brown"       "Ben"
   "Abhishek Raghunathan" "Abhishek"
   "sid"                  "Sid"})


(defn extract-blocks-by-user [user block-uid]
  (q '[:find (pull ?bc [{:block/children ...} :block/string :block/uid])
       :in $ ?user ?buid
       :where
       [?be :block/uid ?buid]
       [?be :block/children ?bc]
       [?bc :block/string ?user]]
    user
    block-uid))

(defn extract-lab-updates [user]
  (let [page-uid (title->uid "Group Meetings")
        latest-meeting (first (sort-by :order (:children (get-child-with-str
                                                           page-uid
                                                           "{{Create Today's Meeting:SmartBlock:Group Meeting Template:RemoveButton=false}}"))))
        latest-meeting-uid (:uid latest-meeting)
        latest-meeting-string (:string latest-meeting)
        lab-updates    (:uid (get-child-with-str
                               latest-meeting-uid
                               "Lab Updates"))
        round-table (:uid (get-child-with-str
                            latest-meeting-uid
                            "Round Table: \"Can't let it go\" and Weekly Recap"))
        template-name  (get db->template-username user)
        user-notes     (ffirst (extract-blocks-by-user
                                 template-name
                                 round-table))]
    (println "lab updates" lab-updates "round table" round-table)
    {:user-notes            user-notes
     :user-notes-uid        (:uid user-notes)
     :lab-updates-uid       lab-updates
     :latest-meeting-uid    latest-meeting-uid
     :latest-meeting-string latest-meeting-string}))

(defn extract-latest-one-on-one-meeting-notes [username]
  (let [page-uid              (title->uid (str "Meetings Sid and Matt"))
        latest-meeting        (first
                                (sort-by
                                  :order
                                  (:children (get-child-with-str
                                               page-uid
                                               "{{Create Today's Entry:SmartBlock:UserDNPToday:RemoveButton=false}} {{Pick A Day:SmartBlock:UserDNPDateSelect:RemoveButton=false}}"))))
        latest-meeting-uid    (:uid latest-meeting)
        latest-meeting-string (:string latest-meeting)]
     {:latest-meeting-uid    latest-meeting-uid
      :latest-meeting-string latest-meeting-string}))


(comment
  (extract-blocks-by-user "Valerie" "B6HTSfziK")
  (extract-lab-updates "Valerie Bentivegna")

  (let [username                  (:title (get-current-user))
        {:keys
         [user-notes
          user-notes-uid
          lab-updates-uid
          latest-meeting-uid
          latest-meeting-string]} (extract-lab-updates username)
        nodes                     {:children [{:string
                                               (str "((" lab-updates-uid "))")}]}
        user-nodes                {:children [{:string
                                               (str "((" user-notes-uid "))")}]}
        vision?                   true
        lab-update-notes          (extract-query-pages
                                    {:context              nodes
                                     :get-linked-refs?     true
                                     :extract-query-pages? false
                                     :only-pages?          false
                                     :vision?              vision?})
        user-plain-notes         (extract-query-pages
                                   {:context              user-nodes
                                    :get-linked-refs?     true
                                    :extract-query-pages? false
                                    :only-pages?          false
                                    :vision?              vision?})
        ref-get-context-uid       (:uid (get-child-with-str
                                          (block-has-child-with-str?
                                            (title->uid "LLM chat settings")
                                            "Quick action buttons")
                                          "Ref relevant notes"))
        step-1-prompt              (get-child-of-child-with-str ref-get-context-uid "Prompt" "Step-1")
        prompt                     (str step-1-prompt
                                     "\n"
                                     lab-update-notes
                                     "\n"
                                     "Individual notes from member: "
                                     user-plain-notes)
        context                    (if vision?
                                     (vec
                                       (concat
                                         [{:type "text"
                                           :text (str step-1-prompt)}]
                                         lab-update-notes
                                         ["Individual notes from member: "]
                                         user-plain-notes))
                                     prompt)
        user-daily-notes-page     (title->uid (str username "/Home"))
        create-dnp-block-uid      (block-has-child-with-str?
                                    user-daily-notes-page
                                    "\uD83D\uDCDD Daily notes {{Create Today's Entry:SmartBlock:UserDNPToday:RemoveButton=false}} {{Pick A Day:SmartBlock:UserDNPDateSelect:RemoveButton=false}}")
        same-latest-dnp-uid?      (block-has-child-with-str?
                                    create-dnp-block-uid
                                    latest-meeting-string)
        top-parent                (if (nil? same-latest-dnp-uid?)
                                    create-dnp-block-uid
                                    same-latest-dnp-uid?)
        parent-block-uid          (gen-new-uid)
        res-block-uid             (gen-new-uid)
        struct                    (if (nil? same-latest-dnp-uid?)
                                    {:s latest-meeting-string
                                     :o "first"
                                     :u parent-block-uid
                                     :c [{:s (str "Reference Group meeting notes for: ((" latest-meeting-uid "))  group meeting")
                                          :c [{:s ""
                                               :u res-block-uid}]}]}
                                    {:s (str "Reference Group meeting notes for: ((" latest-meeting-uid ")) group meeting")
                                     :u parent-block-uid
                                     :c [{:s ""
                                          :u res-block-uid}]})]
    (println "latest meeting string")
    (create-struct
      struct
      top-parent
      res-block-uid
      false
      (p "Ref relevant notes"))))

(comment
  (extract-latest-one-on-one-meeting-notes "sid"))

(defn is-discourse-node? [s]
  (let [nodes ["QUE" "CLM" "EVD" "RES" "ISS" "HYP" "CON"]
        patterns (map #(re-pattern (str "^\\[\\[" % "\\]\\] - (.*?)$")) nodes)
        evd-pattern (re-pattern "^\\[\\[EVD\\]\\] - (.*?) - (.*?)$")
        at-pattern (re-pattern "^@(.*?)$")]
    (or (re-matches evd-pattern s)
      (some #(re-matches % s) patterns)
      (re-matches at-pattern s))))



(defn prior-work-button
  [default-model
   default-temp
   default-max-tokens
   get-linked-refs?
   extract-query-pages?
   extract-query-pages-ref?
   step-1-prompt
   active?]
  (let [disabled? (r/atom true)]
    (println "prior work" active? disabled?)
    (fn []
      (let [mutation-callback (fn mutation-callback [mutations observer]
                                (doseq [mutation mutations]
                                  (when (= (.-type mutation) "childList")
                                    (let [title-element (.querySelector js/document "h1.rm-title-display span")
                                          page-title    (when title-element (.-textContent title-element))
                                          dg-node?      (if (some? page-title)
                                                          (some? (is-discourse-node? page-title))
                                                          false)]
                                      ;(println "Mutation page" page-title "::" dg-node? @disabled?)
                                      (if dg-node?
                                        (reset! disabled? false)
                                        (reset! disabled? true))))))
            star-observing    (let [observer (js/MutationObserver. mutation-callback)]
                                (.observe observer js/document #js {:childList true
                                                                    :subtree true}))])
      (when (not @disabled?)
        [button-with-tooltip
         "The llm summarises what people have already done in this area, based on the notes from current node, linked notes and semantically linked nodes."
         [:> Button {:class-name "one on one meetings"
                     :minimal    true
                     :small      true
                     :loading    @active?
                     :on-click   (fn [e]
                                    (when (not @active?)
                                      (reset! active? true))
                                    (go
                                      (let [current-page-uid    (<p! (get-open-page-uid))
                                            title               (str "[[" (uid->title current-page-uid) "]]")
                                            nodes               {:children
                                                                 [{:string title}]}
                                            vision?             (= "gpt-4-vision" @default-model)
                                            context             (extract-query-pages
                                                                  {:context              nodes
                                                                   :get-linked-refs?     @get-linked-refs?
                                                                   :extract-query-pages? @extract-query-pages?
                                                                   :only-pages?          @extract-query-pages-ref?
                                                                   :vision?              vision?})
                                            settings             {:model       (get model-mappings @default-model)
                                                                  :temperature @default-temp
                                                                  :max-tokens  @default-max-tokens}
                                            parent-block-uid     (gen-new-uid)
                                            stage-1-block-uid    (gen-new-uid)
                                            res-block-uid        (gen-new-uid)
                                            already-context?    (block-has-child-with-str? current-page-uid "AI: Prior work")
                                            top-parent          (if (nil? already-context?)
                                                                   current-page-uid
                                                                   already-context?)
                                            struct              (if (nil? already-context?)
                                                                  {:s "AI: Prior work"
                                                                   :u parent-block-uid
                                                                   :c [{:s ""
                                                                        :u res-block-uid}]}
                                                                  {:s ""
                                                                   :u res-block-uid})]
                                        (do
                                          (create-struct
                                           struct
                                           top-parent
                                           parent-block-uid
                                           true
                                           (p "Ref relevant notes"))
                                          (let [url "https://roam-llm-chat-falling-haze-86.fly.dev/get-openai-embeddings"
                                                headers {"Content-Type" "application/json"}
                                                res (<! (http/post url {:with-credentials? false
                                                                        :headers headers
                                                                        :json-params {:input [title]
                                                                                      :top-k 3}}))
                                                embeddings (first (js->clj (:body res) :keywordize-keys true))
                                                lab-members  (mapv
                                                               (fn [mdata]
                                                                 (-> mdata
                                                                   first
                                                                   :title))
                                                               (get-all-users))
                                                _ (cljs.pprint/pprint embeddings)
                                                similar-nodes-titles {:children (into []
                                                                                  (mapv
                                                                                    (fn [node]
                                                                                      {:string (str "[["
                                                                                                 (-> node :metadata :title)
                                                                                                 "]]")})
                                                                                    embeddings))}
                                                all-embedding-notes   (extract-query-pages
                                                                          {:context              similar-nodes-titles
                                                                           :get-linked-refs?     @get-linked-refs?
                                                                           :extract-query-pages? @extract-query-pages?
                                                                           :only-pages?          @extract-query-pages-ref?
                                                                           :vision?              vision?})

                                                stage-2-prompt        (str step-1-prompt
                                                                              "\n"
                                                                              "<lab-members>"
                                                                               lab-members
                                                                              "</lab-members>"
                                                                              "<current-discourse-node>"
                                                                              context
                                                                              "</current-discourse-node>"
                                                                              "<semantically-similar-nodes-data>"
                                                                              all-embedding-notes
                                                                              "</semantically-similar-nodes-data>")
                                                llm-context           [{:role "user"
                                                                        :content stage-2-prompt}]]
                                            (call-llm-api
                                                    {:messages llm-context
                                                     :settings settings
                                                     :callback   (fn [response]
                                                                   (p (str "Prior work llm response received: " response)
                                                                     (let [res-str (-> response
                                                                                     :body)]
                                                                       (update-block-string
                                                                         res-block-uid
                                                                         (str res-str)
                                                                         (js/setTimeout
                                                                           (fn []
                                                                             (p (str "Prior work Updated block " res-block-uid " with response from openai api"))
                                                                             (reset! active? false))
                                                                           500)))))}))))))}
          "Summarise prior work"]]))))


(defn ooo-meetings-button [default-model
                              default-temp
                              default-max-tokens
                              get-linked-refs?
                              extract-query-pages?
                              extract-query-pages-ref?
                              step-1-prompt
                              step-2-prompt
                              active?]
  (let [disabled? (r/atom true)]
    (fn []
      (let [mutation-callback (fn mutation-callback [mutations observer]
                                (doseq [mutation mutations]
                                  (when (= (.-type mutation) "childList")
                                    (let [current-user  (:title (get-current-user))
                                          name-in-meetings-page (db->meetings-username current-user)
                                          title-element (.querySelector js/document "h1.rm-title-display span")
                                          page-title    (when title-element (.-textContent title-element))]
                                      ;(println "Mutation page" page-title "::" name-in-meetings-page)
                                      (if (and (some? page-title)
                                            (or
                                             (= page-title (str current-user "/Home"))
                                             (= page-title (str "Meetings " name-in-meetings-page " and Matt"))))
                                        (reset! disabled? false)
                                        (reset! disabled? true))))))
            star-observing    (let [observer (js/MutationObserver. mutation-callback)]
                                (.observe observer js/document #js {:childList true
                                                                    :subtree true}))])
      (when (not @disabled?)
        [button-with-tooltip
         "The llm summarises your last one-on-one meeting with Matt. It extracts the action items, blockers and discourse nodes you are working on. "
         [:> Button {:class-name "one on one meetings"
                     :minimal    true
                     :small      true
                     :loading    @active?
                     :on-click (fn [e]
                                 (when (not @active?)
                                   (reset! active? true))
                                 (go
                                   (let [current-user-name          (:title (get-current-user))
                                         {:keys
                                           [latest-meeting-uid
                                            latest-meeting-string]} (extract-latest-one-on-one-meeting-notes current-user-name)
                                         vision?                    (= "gpt-4-vision" @default-model)
                                         latest-meeting-nodes       {:children [{:string
                                                                                 (str "((" latest-meeting-uid "))")}]}
                                         latest-meeting-notes       (extract-query-pages
                                                                      {:context              latest-meeting-nodes
                                                                       :get-linked-refs?     @get-linked-refs?
                                                                       :extract-query-pages? @extract-query-pages?
                                                                       :only-pages?          @extract-query-pages-ref?
                                                                       :vision?              vision?})
                                         stage-1-prompt             (str step-1-prompt
                                                                      "\n"
                                                                      latest-meeting-notes)
                                         stage-1-context           (if vision?
                                                                     (vec
                                                                       (concat
                                                                         [{:type "text"
                                                                           :text (str step-1-prompt)}]
                                                                         latest-meeting-notes))
                                                                     stage-1-prompt)
                                         llm-context               [{:role "user"
                                                                     :content stage-1-context}]
                                         settings                  {:model       (get model-mappings @default-model)
                                                                    :temperature @default-temp
                                                                    :max-tokens  @default-max-tokens}
                                         parent-block-uid          (gen-new-uid)
                                         stage-1-block-uid         (gen-new-uid)
                                         user-daily-notes-page     (title->uid (str current-user-name "/Home"))
                                         create-dnp-block-uid      (block-has-child-with-str?
                                                                     user-daily-notes-page
                                                                     "Summary of last 1:1 meeting with Matt"
                                                                     #_"\uD83D\uDCDD Daily notes {{Create Today's Entry:SmartBlock:UserDNPToday:RemoveButton=false}} {{Pick A Day:SmartBlock:UserDNPDateSelect:RemoveButton=false}}")
                                         same-latest-dnp-uid?      (block-has-child-with-str?
                                                                     create-dnp-block-uid
                                                                     latest-meeting-string)
                                         top-parent                (if (nil? same-latest-dnp-uid?)
                                                                     create-dnp-block-uid
                                                                     same-latest-dnp-uid?)
                                         struct                    (if (nil? same-latest-dnp-uid?)
                                                                     {:s latest-meeting-string
                                                                      :u parent-block-uid
                                                                      :c [{:s (str "Summary of last meeting with Matt on: ((" latest-meeting-uid "))")
                                                                           :c [{:s ""
                                                                                :u stage-1-block-uid}]}]}
                                                                     {:s (str "Summary of last meeting with Matt on: ((" latest-meeting-uid "))")
                                                                      :u parent-block-uid
                                                                      :c [{:s ""
                                                                           :u stage-1-block-uid}]})]
                                     (do
                                       (create-struct
                                         struct
                                         top-parent
                                         parent-block-uid
                                         true
                                         (p "Ref relevant notes"))
                                       (<p! (-> (js/Promise.
                                                  (fn [resolve _]
                                                    (call-llm-api
                                                      {:messages llm-context
                                                       :settings settings
                                                       :callback (fn [response]
                                                                   (let [res-str (-> response :body)]
                                                                     (println "ONE ON ONE MEeting :::: " res-str)
                                                                     (update-block-string
                                                                       stage-1-block-uid
                                                                       (str res-str))
                                                                     (resolve (str res-str))))})))))))))}

          "Summarise last 1:1 meeting"]]))))

(defn filtered-pages-button1
  [default-model
   default-temp
   default-max-tokens
   get-linked-refs?
   extract-query-pages?
   extract-query-pages-ref?
   step-1-prompt
   step-2-prompt
   image-prompt
   active?]
  (let [disabled? (r/atom true)]
    (fn []
      (let [mutation-callback (fn mutation-callback [mutations observer]
                                (doseq [mutation mutations]
                                  (when (= (.-type mutation) "childList")
                                    (let [current-user  (:title (get-current-user))
                                          title-element (.querySelector js/document "h1.rm-title-display span")
                                          page-title    (when title-element (.-textContent title-element))]
                                       (if (and (some? page-title)
                                             (or
                                                (= page-title (str current-user "/Home"))
                                                (= page-title "Group Meetings")))
                                         (reset! disabled? false)
                                         (reset! disabled? true))))))

            star-observing    (let [observer (js/MutationObserver. mutation-callback)]
                                (.observe observer js/document #js {:childList true
                                                                    :subtree true}))])
      (when (not @disabled?)
        [button-with-tooltip
         "The llm extracts the relevant notes from the lab updates and references them in your daily notes page."
         [:> Button {:class-name "chat-with-filtered-pages"
                     :minimal true
                     :small true
                     :loading @active?
                     :on-click (fn [e]
                                 (when (not @active?)
                                   (reset! active? true))
                                 (go
                                   (let [current-user              (get-current-user)
                                         current-user-name         (:title (get-current-user))
                                         current-user-id           (:id (get-current-user))
                                         {:keys
                                          [user-notes
                                           user-notes-uid
                                           lab-updates-uid
                                           latest-meeting-uid
                                           latest-meeting-string]} (extract-lab-updates current-user-name)
                                         vision?                   (= "gpt-4-vision" @default-model)
                                         lab-update-nodes          {:children [{:string
                                                                                (str "((" lab-updates-uid "))")}]}
                                         user-nodes                {:children [{:string
                                                                                (str "((" user-notes-uid "))")}]}
                                         lab-update-notes          (extract-query-pages
                                                                     {:context              lab-update-nodes
                                                                      :get-linked-refs?     @get-linked-refs?
                                                                      :extract-query-pages? @extract-query-pages?
                                                                      :only-pages?          @extract-query-pages-ref?
                                                                      :vision?              vision?})
                                         user-plain-notes          (extract-query-pages
                                                                     {:context              user-nodes
                                                                      :get-linked-refs?     @get-linked-refs?
                                                                      :extract-query-pages? @extract-query-pages?
                                                                      :only-pages?          @extract-query-pages-ref?
                                                                      :vision?              vision?})
                                         stage-1-prompt            (str step-1-prompt
                                                                     "\n"
                                                                     lab-update-notes
                                                                     "\n"
                                                                     "Individual notes from member: "
                                                                     user-plain-notes)
                                         stage-1-context          (if vision?
                                                                    (vec
                                                                      (concat
                                                                        [{:type "text"
                                                                          :text (str step-1-prompt)}]
                                                                        lab-update-notes
                                                                        ["Individual notes from member: "]
                                                                        user-plain-notes))
                                                                    stage-1-prompt)
                                         llm-context               [{:role "user"
                                                                     :content stage-1-context}]
                                         settings                  {:model       (get model-mappings @default-model)
                                                                    :temperature @default-temp
                                                                    :max-tokens  @default-max-tokens}
                                         parent-block-uid          (gen-new-uid)
                                         ref-block-uid             (gen-new-uid)
                                         stage-1-block-uid         (gen-new-uid)
                                         stage-2-block-uid         (gen-new-uid)
                                         user-daily-notes-page     (title->uid (str current-user-name "/Home"))
                                         create-dnp-block-uid      (block-has-child-with-str?
                                                                     user-daily-notes-page
                                                                     "\uD83D\uDCDD Daily notes {{Create Today's Entry:SmartBlock:UserDNPToday:RemoveButton=false}} {{Pick A Day:SmartBlock:UserDNPDateSelect:RemoveButton=false}}")
                                         same-latest-dnp-uid?      (block-has-child-with-str?
                                                                     create-dnp-block-uid
                                                                     latest-meeting-string)
                                         top-parent                (if (nil? same-latest-dnp-uid?)
                                                                     create-dnp-block-uid
                                                                     same-latest-dnp-uid?)
                                         struct                    (if (nil? same-latest-dnp-uid?)
                                                                      {:s latest-meeting-string
                                                                       :u parent-block-uid
                                                                       :c [{:s (str "Reference Group meeting notes for: ((" latest-meeting-uid ")) group meeting")
                                                                            :c [{:s "Your weekly recap"
                                                                                 :c [{:s (str "{{[[embed]]: (("user-notes-uid "))}}")}]}
                                                                                {:s "Meeting summary"
                                                                                 :c [{:s ""
                                                                                      :u stage-1-block-uid}]}
                                                                                {:s "Relevant references from meeting:"
                                                                                 :u ref-block-uid
                                                                                 :c [{:s ""
                                                                                      :u stage-2-block-uid}]}]}]}
                                                                     {:s (str "Reference Group meeting notes for: ((" latest-meeting-uid ")) group meeting")
                                                                      :u parent-block-uid
                                                                      :c [{:s "Your weekly recap"
                                                                           :c [{:s (str "{{[[embed]]: (("user-notes-uid "))}}")}]}
                                                                          {:s "Meeting summary:"
                                                                           :c [{:s ""
                                                                                :u stage-1-block-uid}]}
                                                                          {:s "Relevant references from meeting:"
                                                                           :u ref-block-uid
                                                                           :c [{:s ""
                                                                                :u stage-2-block-uid}]}]})
                                         all-images-in-latest-meeting (get-all-images-for-node
                                                                        latest-meeting-uid
                                                                        true)
                                         image-count                  (count all-images-in-latest-meeting)]
                                     (do
                                       (create-struct
                                        struct
                                        top-parent
                                        parent-block-uid
                                        true
                                        (p "Ref relevant notes"))
                                       (<p! (-> (js/Promise.
                                                  (fn [resolve _]
                                                    (call-llm-api
                                                      {:messages llm-context
                                                       :settings settings
                                                       :callback (fn [response]
                                                                   (let [res-str (-> response :body)]
                                                                     (println "Stage 1 :::: " res-str)
                                                                     (update-block-string
                                                                       stage-1-block-uid
                                                                       (str res-str))
                                                                     (resolve (str res-str))))})))
                                                (.then (fn [stage-1-res]
                                                         (-> (image-to-text-for
                                                                 all-images-in-latest-meeting
                                                                 (atom image-count)
                                                                 (atom true)
                                                                 image-prompt
                                                                 (atom  400))
                                                           (.then (fn [_] stage-1-res)))))
                                                (.then (fn [stage-1-res]
                                                         (let [raw-lab-update-notes (ffirst (q '[:find (pull ?eid [{:block/children ...} :block/string :block/order :block/uid])
                                                                                                 :in $ ?eid]
                                                                                              (uid->eid lab-updates-uid)))
                                                               stage-2-prompt        (str step-2-prompt
                                                                                       "\n"
                                                                                       "Raw data: "
                                                                                       raw-lab-update-notes
                                                                                       "\n"
                                                                                       "Summary: "
                                                                                       stage-1-res)
                                                               llm-context           [{:role "user"
                                                                                       :content stage-2-prompt}]]
                                                            (println stage-2-prompt)
                                                            (call-llm-api
                                                              {:messages llm-context
                                                               :settings settings
                                                               :callback (fn [response]
                                                                           (let [suggestions (js->clj (js/JSON.parse (-> response
                                                                                                                       :body
                                                                                                                       (str/replace #"```json\s*" "") ; Remove ```json
                                                                                                                       (str/replace #"```\s*$" "")    ; Remove trailing ```
                                                                                                                       str/trim
                                                                                                                       extract-from-code-block))
                                                                                               :keywordize-keys true)]
                                                                             (println "Stage 2 :::: " suggestions ref-block-uid)
                                                                             (doseq [sug suggestions]
                                                                               (create-new-block
                                                                                 ref-block-uid
                                                                                 "last"
                                                                                 (str "((" (:uid sug) "))")
                                                                                 #()))
                                                                             (js/setTimeout
                                                                               (fn [] (reset! active? false))
                                                                               500)))}))))))))))}
          "Reference relevant notes"]]))))


(defn bottom-bar-buttons []
  (js/console.time "bottom bar setup")
  (let [dgp-block-uid                (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
        dgp-discourse-graph-page-uid (:uid (get-child-with-str dgp-block-uid "Discourse graph this page"))
        dgp-data                     (-> dgp-discourse-graph-page-uid
                                       (pull-deep-block-data)
                                       extract-data)
        dgp-default-model            (r/atom (:model dgp-data))
        dgp-default-temp             (r/atom (:temperature dgp-data))
        dgp-default-max-tokens       (r/atom (:max-tokens dgp-data))
        dgp-get-linked-refs?         (r/atom (:get-linked-refs? dgp-data))
        dgp-extract-query-pages?     (r/atom (:extract-query-pages? dgp-data))
        dgp-extract-query-pages-ref? (r/atom (:extract-query-pages-ref? dgp-data))
        dgp-active?                  (r/atom (:active? dgp-data))
        dgp-context                  (r/atom (:context dgp-data))



        co-get-context-uid           (:uid (get-child-with-str
                                             (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                             "Get context"))
        co-get-context-data         (-> co-get-context-uid
                                      (pull-deep-block-data)
                                      extract-data)
        co-default-temp             (r/atom (:temperature co-get-context-data))
        co-default-model            (r/atom (:model co-get-context-data))
        co-default-max-tokens       (r/atom (:max-tokens co-get-context-data))
        co-get-linked-refs?         (r/atom (:get-linked-refs? co-get-context-data))
        co-extract-query-pages?     (r/atom (:extract-query-pages? co-get-context-data))
        co-extract-query-pages-ref? (r/atom (:extract-query-pages-ref? co-get-context-data))
        co-pre-prompt               (:pre-prompt co-get-context-data)
        co-remaining-prompt         (:further-instructions co-get-context-data)
        co-active?                  (r/atom (:active? co-get-context-data))


        sug-get-context-uid          (:uid (get-child-with-str
                                             (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                             "Get suggestions"))
        sug-get-context-data         (-> sug-get-context-uid
                                       (pull-deep-block-data)
                                       extract-data)
        sug-default-model            (r/atom (:model sug-get-context-data))
        sug-default-temp             (r/atom (:temperature sug-get-context-data))
        sug-default-max-tokens       (r/atom (:max-tokens sug-get-context-data))
        sug-get-linked-refs?         (r/atom (:get-linked-refs? sug-get-context-data))
        sug-extract-query-pages?     (r/atom (:extract-query-pages? sug-get-context-data))
        sug-extract-query-pages-ref? (r/atom (:extract-query-pages-ref? sug-get-context-data))
        sug-pre-prompt               (:pre-prompt sug-get-context-data)
        sug-remaining-prompt         (:further-instructions sug-get-context-data)
        sug-active?                  (r/atom (:active? sug-get-context-data))

        ref-get-context-uid          (:uid (get-child-with-str
                                             (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                             "Ref relevant notes"))
        ref-default-model            (r/atom (get-child-of-child-with-str ref-get-context-uid "Settings" "Model"))
        ref-default-temp             (r/atom (js/parseFloat (get-child-of-child-with-str ref-get-context-uid "Settings" "Temperature")))
        ref-default-max-tokens       (r/atom (js/parseInt (get-child-of-child-with-str ref-get-context-uid "Settings" "Max tokens")))
        ref-get-linked-refs?         (r/atom (if (= "true" (get-child-of-child-with-str ref-get-context-uid "Settings" "Get linked refs"))
                                               true
                                               false))
        ref-extract-query-pages?     (r/atom (if (= "true" (get-child-of-child-with-str ref-get-context-uid "Settings" "Extract query pages"))
                                               true
                                               false))
        ref-extract-query-pages-ref? (r/atom (if (= "true" (get-child-of-child-with-str ref-get-context-uid "Settings" "Extract query pages ref?"))
                                               true
                                               false))
        ref-step-1-prompt            (get-child-of-child-with-str ref-get-context-uid "Prompt" "Step-1")
        ref-step-2-prompt            (get-child-of-child-with-str ref-get-context-uid "Prompt" "Step-2")
        ref-image-prompt             (get-child-of-child-with-str ref-get-context-uid "Prompt" "Image-prompt")
        ref-active?                  (r/atom false)


        ooo-get-context-uid          (:uid (get-child-with-str
                                            (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                            "One on one meetings"))
        ooo-default-model            (r/atom (get-child-of-child-with-str ooo-get-context-uid "Settings" "Model"))
        ooo-default-temp             (r/atom (js/parseFloat (get-child-of-child-with-str ooo-get-context-uid "Settings" "Temperature")))
        ooo-default-max-tokens       (r/atom (js/parseInt (get-child-of-child-with-str ooo-get-context-uid "Settings" "Max tokens")))
        ooo-get-linked-refs?         (r/atom (if (= "true" (get-child-of-child-with-str ooo-get-context-uid "Settings" "Get linked refs"))
                                               true
                                               false))
        ooo-extract-query-pages?     (r/atom (if (= "true" (get-child-of-child-with-str ooo-get-context-uid "Settings" "Extract query pages"))
                                               true
                                               false))
        ooo-extract-query-pages-ref? (r/atom (if (= "true" (get-child-of-child-with-str ooo-get-context-uid "Settings" "Extract query pages ref?"))
                                               true
                                               false))
        ooo-step-1-prompt            (get-child-of-child-with-str ooo-get-context-uid "Prompt" "Step-1")
        ooo-step-2-prompt            (get-child-of-child-with-str ooo-get-context-uid "Prompt" "Step-2")
        ooo-active?                  (r/atom false)


        prior-work-done-get-context-uid          (:uid (get-child-with-str
                                                         (block-has-child-with-str? (title->uid "LLM chat settings") "Quick action buttons")
                                                         "Prior work"))
        prior-work-default-model            (r/atom (get-child-of-child-with-str prior-work-done-get-context-uid "Settings" "Model"))
        prior-work-default-temp             (r/atom (js/parseFloat (get-child-of-child-with-str prior-work-done-get-context-uid "Settings" "Temperature")))
        prior-work-default-max-tokens       (r/atom (js/parseInt (get-child-of-child-with-str prior-work-done-get-context-uid "Settings" "Max tokens")))
        prior-work-get-linked-refs?         (r/atom (if (= "true" (get-child-of-child-with-str prior-work-done-get-context-uid "Settings" "Get linked refs"))
                                                      true
                                                      false))
        prior-work-extract-query-pages?     (r/atom (if (= "true" (get-child-of-child-with-str prior-work-done-get-context-uid "Settings" "Extract query pages"))
                                                      true
                                                      false))
        prior-work-extract-query-pages-ref? (r/atom (if (= "true" (get-child-of-child-with-str prior-work-done-get-context-uid "Settings" "Extract query pages ref?"))
                                                        true
                                                        false))
        prior-work-step-1-prompt            (get-child-of-child-with-str prior-work-done-get-context-uid "Prompt" "Step-1")
        prior-work-step-2-prompt            (get-child-of-child-with-str prior-work-done-get-context-uid "Prompt" "Step-2")
        prior-work-active?                  (r/atom false)]
   (fn []
      (p "Render bottom bar buttons")
      [:> ButtonGroup
       {:style {:display         "flex"
                :justify-content "center"
                :align-items     "center"
                :width           "100%"}
        :fill true}
       [:> ButtonGroup
        {:class-name "button-with-settings"
         :style {:overflow "hidden"
                 :display "flex"
                 :flex-direction "row"
                 :justify-content "space-between"
                 :align-items "center"
                 :flex "1 1 1"}
         :minimal true}
        [:div {:class-name "Classes.POPOVER_DISMISS_OVERRIDE"
               :style {:flex "1 1 1"}}
         [:> Popover
               [:> Button {:icon "cog"
                               :minimal true
                               :small true
                               :style {:background-color "#eeebeb"}}]
          [:> Menu
           {:style {:padding "20px"}
            :class-name "Classes.POPOVER_DISMISS_OVERRIDE"}
           [buttons-settings
            "Discourse graph this page"
            dgp-discourse-graph-page-uid
            dgp-default-temp
            dgp-default-model
            dgp-default-max-tokens
            dgp-get-linked-refs?
            dgp-extract-query-pages?
            dgp-extract-query-pages-ref?]

           [buttons-settings
             "Get Context"
             co-get-context-uid
             co-default-temp
             co-default-model
             co-default-max-tokens
             co-get-linked-refs?
             co-extract-query-pages?
             co-extract-query-pages-ref?]
           [buttons-settings
            "Get Suggestions"
            sug-get-context-uid
            sug-default-temp
            sug-default-model
            sug-default-max-tokens
            sug-get-linked-refs?
            sug-extract-query-pages?
            sug-extract-query-pages-ref?]]]]]


       [discourse-graph-this-page-button
        dgp-block-uid
        dgp-default-model
        dgp-default-temp
        dgp-default-max-tokens
        dgp-get-linked-refs?
        dgp-extract-query-pages?
        dgp-extract-query-pages-ref?
        dgp-active?
        dgp-context]


       [:> Divider]
       [get-context-button
        nil
        co-default-model
        co-default-temp
        co-default-max-tokens
        co-get-linked-refs?
        co-extract-query-pages?
        co-extract-query-pages-ref?
        co-active?
        co-pre-prompt
        co-remaining-prompt]

       [:> Divider]
       [:div.search-pinecone
        {:style {:flex "0 0 50%"}}
        [button-with-tooltip
         "Do semantic search over existing Discourse graph nodes. Type your query and press the send button.
        We get top 3 results, select any one to go to that query result page. "
         [search-pinecone]]]
       [:> Divider]


       [get-suggestions-button
        nil
        sug-default-model
        sug-default-temp
        sug-default-max-tokens
        sug-get-linked-refs?
        sug-extract-query-pages?
        sug-extract-query-pages-ref?
        sug-active?
        sug-pre-prompt
        sug-remaining-prompt]

       #_[:div {:style {:flex "1 1 1"}}
          [button-with-settings "Summarise this page"]]
       [:> Divider]
       [:div
        {:style {:flex "1 1 1"}}
        [button-with-tooltip
         "Using the content of the current page (including zoomed-in pages/blocks) as context, start a conversation with your selected LLM.
        Control the LLM model, response length, and temperature in the interface that is created by this button."
         [:> Button {:minimal true
                     :small true
                     :style {:flex "1 1 1"}
                     :on-click (fn [e]
                                 ;; UPDATE THIS CODE
                                 (p "*Chat with this page* :button clicked")
                                 (go
                                   (let [pre            "*Chat with this page* :"
                                         chat-block-uid (gen-new-uid)
                                         open-page-uid (<p! (get-open-page-uid))
                                         page-title    (uid->title open-page-uid)
                                         context       (if (nil? page-title)
                                                         (str "((" open-page-uid "))")
                                                         (str "[[" page-title "]]"))
                                         context-struct [{:s context}
                                                         {:s ""}]
                                         ai-block? (ai-block-exists? open-page-uid)]
                                     (p (str pre "block with `AI chats` exist? " ai-block?))
                                     (p (str pre "context" context))
                                     ;(println "open page uid" open-page-uid)
                                     ;(println "page title" page-title)
                                     ;(println "extract block" block-data)
                                     (if (some? ai-block?)
                                       (do
                                        (js/console.time "Chat with this page")
                                        (create-struct
                                          (default-chat-struct chat-block-uid  nil nil context-struct)
                                          ai-block?
                                          chat-block-uid
                                          true
                                          (p (str pre "Created a new chat block and opening in sidebar with context: " context)))
                                        (js/console.timeEnd "Chat with this page"))
                                      (do
                                        (js/console.time "Chat with this page")
                                        (create-struct
                                          (chat-ui-with-context-struct chat-block-uid nil context-struct)
                                          open-page-uid
                                          chat-block-uid
                                          true
                                          (p (str pre "Created a new chat block under `AI chats` block and opening in sidebar with context: " context)))
                                        (js/console.timeEnd "Chat with this page"))))))}
          "Chat with this page"]]]
       #_[:> Divider]
       #_[filtered-pages-button1
          ref-default-model
          ref-default-temp
          ref-default-max-tokens
          ref-get-linked-refs?
          ref-extract-query-pages?
          ref-extract-query-pages-ref?
          ref-step-1-prompt
          ref-step-2-prompt
          ref-image-prompt
          ref-active?]

       #_[:> Divider]
       #_[ooo-meetings-button
          ooo-default-model
          ooo-default-temp
          ooo-default-max-tokens
          ooo-get-linked-refs?
          ooo-extract-query-pages?
          ooo-extract-query-pages-ref?
          ooo-step-1-prompt
          ooo-step-2-prompt
          ooo-active?]

       #_[:> Divider]
       #_[prior-work-button
          prior-work-default-model
          prior-work-default-temp
          prior-work-default-max-tokens
          prior-work-get-linked-refs?
          prior-work-extract-query-pages?
          prior-work-extract-query-pages-ref?
          prior-work-step-1-prompt
          prior-work-active?]
       #_[:div
          {:style {:flex "1 1 1"}}
          [button-with-tooltip
           "Begin a brand new empty chat from any page (including zoomed-in pages/blocks), no context is included. Think of this as a quick chat. The chat block will be added in your daily notes page and the chat window will appear in your right sidebar.
        Choose your LLM and adjust its settings within the chat interface. ."
           [:> Button {:minimal true
                       :small true
                       :on-click (fn [e]
                                   (p "*Start chat in daily notes, show in sidebar* :button clicked")
                                   (let [pre            "*Start chat in daily notes, show in sidebar* :"
                                         chat-block-uid (gen-new-uid)
                                         ai-block?      (ai-block-exists? (get-todays-uid))]
                                     (p (str pre "block with `AI chats` exist? " ai-block?))
                                     (if (some? ai-block?)
                                       (create-struct
                                         (default-chat-struct chat-block-uid)
                                         ai-block?
                                         chat-block-uid
                                         true
                                         (p (str pre "Created a new chat block and opening in sidebar. With no context. ")))
                                       (create-struct
                                         (chat-ui-with-context-struct chat-block-uid)
                                         (get-todays-uid)
                                         chat-block-uid
                                         true
                                         (p (str pre "Created a new chat block under `AI chats` block and opening in sidebar. With no context."))))))}
            "Start new chat"]]]
       #_[:> Divider]
       #_[:div
          {:style {:flex "1 1 1"}}
          [:> Button
           {:minimal true
            :small true
            :on-click (fn [e]
                        (let [url         "https://roam-llm-chat-falling-haze-86.fly.dev/get-openai-embeddings"
                              upsert-data  (clj->js {:input (subvec (all-dg-nodes) 1600)})
                              multiple-query-data   (clj->js {:input ["Myosin plays a critical role in assisting endocytosis under conditions of high membrane tension"
                                                                      #_"Increasing membrane tension from 2 pN/nm to 2000 pN/nm in simulations showed a broader assistance by myosin in internalization"]
                                                              :top-k "8"})
                              single-query-data (clj->js {:input ["Increasing membrane tension from 2 pN/nm to 2000 pN/nm in simulations showed a broader assistance by myosin in internalization
                             Resistance to internalization increased as myosin unbinding rate decreased at higher membrane tension in simulations
                             At 20 pN/nm membrane tension, areas with low myosin unbinding rates had decreased internalization resistance
                            Investigate the relationship between myosin catch bonding parameters and internalization efficiency in live cell experiments
                            Myosin assists more broadly in membrane internalization under higher tension conditions
                            High membrane tension facilitates myosin’s role in overcoming resistance to internalization  "]
                                                          :top-k "3"})



                              headers      {"Content-Type" "application/json"}
                              res-ch       (http/post url {:with-credentials? false
                                                           :headers headers
                                                           :json-params multiple-query-data})]
                          #_(println "SENDING EMBEDDINGS REQUEST" (count (all-dg-nodes))) ""
                          #_(println "DATA : " (take 2 upsert-data))
                          (println "query data" single-query-data)
                          (take! res-ch (fn [res]
                                          (let [embeddings (->> (js->clj (-> res :body ) :keywordize-keys true)
                                                             (map
                                                               (fn [x]
                                                                 (str (-> x :metadata :title) "- Score: " (:score x)))))]
                                            #_(println "GOT EMBEDDINGS :" "--" embeddings))))))}
           "Create embeddings"]]
       #_[:> Divider]
       #_[:div
          {:style {:flex "1 1 1"}}
          [button-with-tooltip
           "Same as `Start new chat` button but starts the chat in the block you are focused on."
           [:> Button {:minimal true
                       :small true
                       :style {:flex "1 1 1"}
                       :on-click (fn [e]
                                   (p "*Start chat in focused block* :button clicked")
                                   (let [pre            "*Start chat in focused block* "
                                         chat-block-uid (gen-new-uid)
                                         [parent-uid
                                          block-order]  (get-block-parent-with-order (get-focused-block))
                                         chat-struct    (chat-ui-with-context-struct chat-block-uid nil nil block-order)]
                                     (create-struct
                                       chat-struct
                                       parent-uid
                                       chat-block-uid
                                       false
                                       (p (str pre "Created a new chat block under focused block and opening in sidebar. With no context.")))))}
            "Start new chat in focused block"]]]
       #_[:> Divider]
       #_[:div
          {:style {:flex "1 1 1"}}
          [discourse-graph-this-page-button]]
       #_[:> Divider]
       #_[:div {:style {:flex "1 1 1"}}
          [filtered-pages-button]]
       #_[:> Divider]
       #_[:div
          {:style {:flex "1 1 1"}}
          [text-to-image-button]]])))

