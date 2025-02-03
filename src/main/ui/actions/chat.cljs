(ns ui.actions.chat
  (:require
    [applied-science.js-interop :as j]
    [reagent.core :as r]
    [ui.utils :refer [replace-block-uids create-alternate-messages count-tokens-api call-llm-api update-block-string-for-block-with-child q p get-parent-parent extract-from-code-block log update-block-string-and-move is-a-page? get-child-with-str move-block create-new-block]]
    [cljs.core.async.interop :as asy :refer [<p!]]
    [ui.extract-data.chat :as ed :refer [extract-query-pages data-for-nodes]]
    [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]))


(defn send-context-and-message [message-atom block-uid active? settings token-count-atom context]
  (p "*load context* send message to llm for uid: " block-uid)
  (let [pre             "*load context* :"
        message-block   (get-child-with-str block-uid "Messages")
        messages        (filterv
                          (fn [n]
                            (not= "{{llm-dg-suggestions}}" (:string n)))
                          (vec (sort-by :order (:children message-block))))
        message-by-role (create-alternate-messages messages context pre)
        m-uid           (:uid message-block)]

    (p (str pre "Calling openai api, with settings : " settings))
    (p (str pre "and messages : " message-by-role))
    (p (str pre "Counting tokens for message:"))
    (count-tokens-api {:message message-by-role
                       :model (:model settings)
                       :token-count-atom token-count-atom
                       :block-uid block-uid})
    (p (str pre "Now sending message and wait for response ....."))

    (call-llm-api
      {:messages message-by-role
       :settings settings
       :callback (fn [response]
                   (p "received response from llm")
                   (p (str pre "openai api response received: " response))
                   (let [res-str (-> response
                                   :body)]
                     (create-new-block m-uid "last" (str "**Assistant:** " res-str) (js/setTimeout
                                                                                      (fn []
                                                                                        #_(println "new block in messages")
                                                                                        (p (str pre "Update token count, after llm response"))
                                                                                        (count-tokens-api {:message res-str
                                                                                                           :model (:model settings)
                                                                                                           :update? true
                                                                                                           :block-uid block-uid
                                                                                                           :token-count-atom token-count-atom})
                                                                                        (p (str pre "Add assistant response block in messages: " m-uid))
                                                                                        (reset! message-atom (get-child-with-str block-uid "Messages"))
                                                                                        (update-block-string-for-block-with-child block-uid "Settings" "Active?" (str (not @active?)))
                                                                                        (reset! active? false))
                                                                                      500))))})))

(comment
  (create-alternate-messages
    (vec (sort-by :order (:children (get-child-with-str "aDKm-_42K" "Messages"))))
    (extract-query-pages
      {:context              {:children [{:order 0, :string "[[Test: Image to text]]", :uid "idyAjL4Xd"}],}
       :get-linked-refs?     true
       :extract-query-pages? true
       :only-pages?          false
       :vision?              true})
    "GG: "))

(defn load-context [{:keys [chat-atom
                            messages-atom
                            parent-id
                            active?
                            get-linked-refs?
                            settings
                            token-count-atom
                            extract-query-pages?
                            extract-query-pages-ref?
                            vision?]}]
  #_(println "load context ")
  (p "*load context* for block with uid:" parent-id "get-linked-refs? " @get-linked-refs? "extract-query-pages? " @extract-query-pages? "extract-query-pages-ref?" @extract-query-pages-ref?)
  (let [pre      "*load context* :"
        messages (get-child-with-str parent-id "Messages")
        chat     (get-child-with-str parent-id "Chat")
        context  (get-child-with-str parent-id "Context")
        m-len    (count (:children messages))
        m-uid    (:uid messages)
        children (:children chat)
        c-uid    (:uid chat)
        ext-context (atom "\n Initial context: \n")]

    (p (str pre "for these: " children))

    (go
      (reset! ext-context (extract-query-pages
                            {:context              context
                             :get-linked-refs?     @get-linked-refs?
                             :extract-query-pages? @extract-query-pages?
                             :only-pages?          @extract-query-pages-ref?
                             :vision?              vision?}))
      (doseq [child children]
        ^{:key child}
        (let [child-uid (:uid child)
              cstr (:string child)
              order (+ m-len (:order child))]
          (do
            #_(println "order ------>" order)
            (cond
              (or (= "{{query block}}"
                    cstr)
                (= "{{ query block }}"
                  cstr))                (<p! (-> (j/call-in js/window [:roamjs :extension :queryBuilder :runQuerySync] child-uid)
                                               (.then (fn [r]
                                                        (p (str pre "This is query block: " cstr))
                                                        (p (str pre "Query result are: " r))
                                                        (let [res (vec (map (fn [m]
                                                                              (is-a-page? m))
                                                                         (js->clj r :keywordize-keys true)))
                                                              page-data (str
                                                                          "```"
                                                                          (clojure.string/join "\n -----" (data-for-nodes
                                                                                                            {:nodes res
                                                                                                             :get-linked-refs? @get-linked-refs?
                                                                                                             :extract-query-pages? @extract-query-pages?}))
                                                                          "```")]
                                                          (p (str pre "extracted data from query pages: " page-data))
                                                          (update-block-string-and-move
                                                            child-uid
                                                            page-data
                                                            m-uid
                                                            order))))))
              (some? (is-a-page? cstr)) (do
                                          (p (str pre "This is a page: " cstr))
                                          (let [page-data (str
                                                            "```"
                                                            (clojure.string/join "\n -----" (data-for-nodes
                                                                                              {:nodes [(is-a-page? cstr)]
                                                                                               :get-linked-refs? @get-linked-refs?
                                                                                               :extract-query-pages? @extract-query-pages?}))
                                                            "```")]
                                            (p (str pre "extracted data for the page: " page-data))
                                            (<p! (update-block-string-and-move
                                                   child-uid
                                                   page-data
                                                   m-uid
                                                   order))))
              :else
                                        (do
                                          (p (str pre "This is a normal block: " cstr))
                                          (<p! (update-block-string-and-move
                                                 child-uid
                                                 (str "**User:** " cstr)
                                                 m-uid
                                                 order)))))))
     (<p! (create-new-block c-uid "first" "" ()))
     (do
       (p (str pre "refresh messages window with parent-id: " parent-id))
       (reset! messages-atom (get-child-with-str parent-id "Messages"))
       #_(println "messages atom reset")
       (send-context-and-message  messages-atom parent-id active? settings token-count-atom @ext-context))
     (do
       (p (str pre "refresh chat window with parent-id: " parent-id))
       (reset! chat-atom (get-child-with-str parent-id "Chat"))))))


