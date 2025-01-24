(ns ui.utils
  (:require
    ["@blueprintjs/core" :as bp :refer [ControlGroup Position Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]
    [cljs.core.async :as async :refer [<! >! go chan put! take! timeout]]
    [cljs.core.async.interop :as asy :refer [<p!]]
    [cljs.reader :as reader]
    [reagent.core :as r]
    [cljs-http.client :as http]
    [applied-science.js-interop :as j]
    [clojure.string :as str]))


(defn log
  [& args]  (apply js/console.log args))

(defn p [& args]
  (apply println (str "CL: ") args))


(def markdown-image-pattern #"!\[([^\]]*)\]\((.*?)\)")
;; ---- Datascript specific ------

(defn q
  ([query]
   (let [serialised-query (if (string? query)
                            query
                            (pr-str query))
         roam-api         (.-data (.-roamAlphaAPI js/window))
         q-fn             (.-q roam-api)]
     (-> (.apply q-fn roam-api (array serialised-query))
       (js->clj :keywordize-keys true))))
  ([query & args]
   (let [serialised-query (if (string? query)
                            query
                            (pr-str query))
         roam-api         (.-data (.-roamAlphaAPI js/window))
         q-fn             (.-q roam-api)]
     (-> (.apply q-fn roam-api (apply array (concat [serialised-query] args)))
       (js->clj :keywordize-keys true)))))


(defn extract-all-discourse-for [node]
  (let [node-regex (re-pattern (cond
                                 (= "EVD" node) (str "^\\[\\[EVD\\]\\] - (.*?) - (.*?)$")
                                 :else (str "^\\[\\[" node "\\]\\] - (.*?)$")))]
    (flatten (q '[:find (pull ?e [:node/title :block/uid])
                  :in $ ?node-regex
                  :where
                  [?e :node/title ?tit]
                  [(re-find ?node-regex ?tit)]]
               node-regex))))


(defn extract-all-sources []
  (let [node-regex (re-pattern "^@[^\\s]+$")]
    (into [] (flatten (q '[:find
                           (pull ?node [:block/string :node/title :block/uid])
                           :where
                           [(re-pattern "^@(.*?)$") ?.*?$-regex]
                           [?node :node/title ?node-Title]
                           [(re-find ?.*?$-regex ?node-Title)]])))))
(extract-all-sources)


(defn all-dg-nodes []
 (let [cnt (atom [])
       nodes ["QUE" "CLM" "EVD" "RES" "ISS" "HYP" "CON"]]
   (doseq [node nodes]
     (let [total  (extract-all-discourse-for node)]
       (swap! cnt concat total)))
   (into [] (concat (into [] @cnt) (extract-all-sources)))))
(count (all-dg-nodes))

(def all-title
  (q '[:find ?u  ?t
       :where [?e :node/title ?t]
       [?e :block/uid ?u]]))

(def dg-nodes
  (filter
    #(let [uid (str (first %))
           dg? (j/call-in js/window [:roamjs :extension :queryBuilder :isDiscourseNode] uid)]
       (when dg?
         %))
    all-title))

(defn node-count [t]
  (->> (filter
         #(let [s (second %)]
            (when (clojure.string/starts-with? s (str "[[" t "]]"))
              (second %)))
         dg-nodes)
    (map second)))


(comment
  (count (node-count "EVD"))
  (node-count "EVD"))

(comment
  (extract-all-discourse-for "EVD")
  (into #{} (node-count "EVD"))
  (clojure.set/difference
     (into #{} (->> (extract-all-discourse-for "EVD")
                 (map :title)))
     (into #{} (node-count "EVD")))
 (count (extract-all-discourse-for "CLM"))
 (count (extract-all-discourse-for "EVD"))
 (count (extract-all-discourse-for "HYP"))
 (count (extract-all-discourse-for "RES"))
 (count (extract-all-discourse-for "ISS"))
 (count (extract-all-discourse-for "QUE"))
 (count (extract-all-discourse-for "EXP"))
 (count (extract-all-discourse-for "CON"))
 (count (q '[:find ?t
             :where [?e :node/title]])))

(defn get-uid-from-localstorage []
  (let [user-data (loop [remaining  (reader/read-string
                                      (.getItem (.-localStorage js/window) "globalAppState"))
                         prev       nil]
                    (if (= "~:user" prev)
                      (first remaining)
                      (recur (rest remaining) (first remaining))))]
    (loop [remaining user-data
           prev nil]
      (when (seq remaining)
        (if (= "~:uid" prev)
          (first remaining)
          (recur (rest remaining) (first remaining)))))))

(defn get-current-user []
  (let [user-uid (get-uid-from-localstorage)
        username (first (flatten (q '[:find (pull ?duid [*])
                                      :in $ ?user-uid
                                      :where
                                      [?eid :user/uid ?user-uid]
                                      [?eid :user/display-page ?duid]
                                      [?duid :node/title ?username]]
                                   user-uid)))]
    username))


(defn get-all-users []
  (q '[:find (pull ?user [*])
       :where
       [?eid :create/user ?uid]
       [?duid :user/display-page ?user]]))

(comment
  (get-all-users)
  (mapv
    (fn [mdata]
     (-> mdata
      first
      :title))
    (get-all-users)))



(defn uid-to-block [uid]
  (q '[:find (pull ?e [*])
       :in $ ?uid
       :where [?e :block/uid ?uid]]
    uid))

(defn uid->title [uid]
  (ffirst (q '[:find ?title
               :in $ ?uid
               :where
               [?e :block/uid ?uid]
               [?e :node/title ?title]]

            uid)))

(defn title->uid [title]
  (ffirst (q '[:find ?uid
               :in $ ?title
               :where
               [?e :node/title ?title]
               [?e :block/uid ?uid]]
            title)))


(defn get-eid [title]
  (ffirst (q '[:find ?eid
               :in $ ?title
               :where [?eid :node/title ?title]]
            title)))

(defn uid->eid [uid]
  (ffirst (q '[:find ?eid
               :in $ ?uid
               :where [?eid :block/uid ?uid]]
            uid)))

(defn is-a-page? [s]
  (second (re-find #"^\[\[(.*?)\]\]$" (clojure.string/trim s))))

(comment
  (is-a-page? " [[[[ISS]] - scope other options for the LLM chat interface]]")
  (is-a-page? "[[EVD]] - NWASP was found around clusters of clathrin heavy chain by TIRF microscopy + super resolution microscopy - [[@leyton-puig2017flat]]"))


(defn extract-from-code-block
  ([s]
   (extract-from-code-block s false))
  ([s v?]
   (let [pattern         #"(?s)```javascript\n \n(.*?)\n```"
         pres?           (re-find pattern s)
         relaxed-pattern #"(?s)```\s*(.*?)\s*```"
         rpres?         (re-find relaxed-pattern s)]
     (cond
       pres?     (str (second pres?) " \n ")
       (and v?
         rpres?) (str (clojure.string/trim (second rpres?)) " \n ")
       :else     (str s " \n ")))))

(comment
  (def t "Here is some code:\n```\nprint('Hello, world!')\n```")
  (def tt "```javascript\n \nconsole.log('Hello, world!');\n```")
  (def n "```
        [[CLM]] - Actin accumulation at sites of endocytosis increases with membrane tension
  [[CLM]] - Increased membrane tension results in slower dynamics of clathrin coated structures.
  [[CLM]] - CG endocytosis is upregulated upon decrease of cell membrane tension.
  ```")
  (extract-from-code-block t)
  (extract-from-code-block tt)
  (extract-from-code-block n))

(defn unnamespace-keys
  [m]
  (let [remove-ns (fn [k] (keyword (name k)))] ; Helper to remove namespace
    (into {}
      (mapv (fn [[k v]]
              [(remove-ns k) ; Remove namespace from key
               (if (map? v)
                 (unnamespace-keys v) ; Recursively process nested maps
                 (if (coll? v)
                   (mapv unnamespace-keys v) ; Apply recursively to each item if v is a collection
                   v))]) ; Leave other values unchanged
        m))))

(defn add-pull-watch
  [pull-pattern entity-id callback]
  (let [roam-api (.-data (.-roamAlphaAPI js/window))
        add-pull-watch-fn (.-addPullWatch roam-api)
        js-callback (fn [before after]
                      (let [b (unnamespace-keys (js->clj before :keywordize-keys true))
                            a (unnamespace-keys (js->clj after :keywordize-keys true))]
                        (callback b a)))]
    (.addPullWatch roam-api pull-pattern entity-id js-callback)))



(defn watch-children [block-uid cb]
  (let [pull-pattern "[:block/uid :block/order :block/string {:block/children ...}]"
        entity-id (str [:block/uid block-uid])]
    (println "add pull watch :" entity-id)
    (add-pull-watch pull-pattern entity-id cb)))

(defn watch-string [block-uid cb]
  (let [pull-pattern "[:block/uid :block/string]"
        entity-id (str [:block/uid block-uid])]
    (p "add string watch :" entity-id)
    (add-pull-watch pull-pattern entity-id cb)))

(defn get-child-with-str  [block-uid s]
  (ffirst (q '[:find (pull ?c [:block/string :block/uid :block/order {:block/children ...}])
               :in $ ?uid ?s
               :where
               [?e :block/uid ?uid]
               [?e :block/children ?c]
               [?c :block/string ?s]]
            block-uid
            s)))


(defn get-child-of-child-with-str
  ([block-uid p1 p2]
   (get-child-of-child-with-str block-uid p1 p2 true))
  ([block-uid p1 p2 only-string?]
   (let [res (ffirst (q '[:find  (pull ?c3 [:block/string :block/uid])
                          :in $ ?uid ?p1 ?p2
                          :where
                          [?e :block/uid ?uid]
                          [?e :block/children ?c1]
                          [?c1 :block/string ?p1]
                          [?c1 :block/children ?c2]
                          [?c2 :block/string ?p2]
                          [?c2 :block/children ?c3]
                          [?c3 :block/string ?p3]]
                       block-uid
                       p1
                       p2))]
      (if only-string?
        (:string res)
        (:uid res)))))


(defn get-child-of-child-with-str-on-page [page p1 p2 p3]
  (ffirst (q '[:find  (pull ?c3 [:block/string :block/uid :block/order {:block/children ...}])
               :in $ ?page ?p1 ?p2 ?p3
               :where
               [?e :node/title ?page]
               [?e :block/children ?c1]
               [?c1 :block/string ?p1]
               [?c1 :block/children ?c2]
               [?c2 :block/string ?p2]
               [?c2 :block/children ?c3]
               [?c3 :block/string ?p3]]
            page
            p1
            p2
            p3)))


(defn get-title-with-uid [title]
  (q '[:find (pull ?e [:node/title :block/uid])
       :in $ ?n
       :where [?e :node/title ?n]]
    title))


(defn get-parent-parent [uid]
  (ffirst (q '[:find  ?p
               :in $ ?uid
               :where [?e :block/uid ?uid]
               [?e :block/parents ?p1]
               [?p1 :block/string "Context"]
               [?p1 :block/parents ?p2]
               [?p2 :block/string "{{ chat-llm }}"]
               [?p2 :block/uid ?p]]
            uid)))

(defn get-block-parent-with-order [block-uid]
  (first (q '[:find ?pu ?o
              :in $ ?uid
              :where [?e :block/uid ?uid]
              [?e :block/parents ?p]
              [?p :block/uid ?pu]
              [?e :block/order ?o]]
           block-uid)))

(defn block-has-child-with-str? [page bstr]
  (ffirst
    (q '[:find ?uid
         :in $ ?today ?bstr
         :where
         [?e :block/uid ?today]
         [?e :block/children ?c]
         [?c :block/string ?bstr]
         [?c :block/uid ?uid]]
      page
      bstr)))


(defn ai-block-exists? [page]
  (block-has-child-with-str? page "AI chats"))

;; --- Roam specific ---

(defn get-todays-uid []
  (->> (js/Date.)
    (j/call-in js/window [:roamAlphaAPI :util :dateToPageUid])))
(get-todays-uid)

(defn gen-new-uid []
  (j/call-in js/window [:roamAlphaAPI :util :generateUID]))

(defn get-open-page-uid []
  (j/call-in js/window [:roamAlphaAPI :ui :mainWindow :getOpenPageOrBlockUid]))

(defn delete-block [uid]
  (j/call-in js/window [:roamAlphaAPI :data :block :delete]
    (clj->js {:block {:uid uid}})))

(comment
  (get-open-page-uid))

(defn remove-entry [block-str]
  (let [patterns ["Entry:SmartBlock:"
                  "\\{\\{Create Today's Entry:SmartBlock"
                  "\\{\\{Pick A Day:SmartBlock"
                  "\\#.sticky"]
        regex-pattern (str/join "|"
                        (map #(str % ".*?(\\s|$)") patterns))]
    (str/replace block-str (re-pattern regex-pattern) "")))


(defn replace-block-uids [block-str]
  (when (some? block-str)
   (let [re (re-pattern "\\(\\(\\(?([^)]*)\\)?\\)\\)")
         matches (re-seq re block-str)]
     (-> (reduce (fn [s [whole-match uid]]

                   (let [replace-str (str
                                       (:string (ffirst (uid-to-block uid))))

                         replacement (if (str/starts-with? whole-match "(((")
                                       (str "(" replace-str ")")
                                       replace-str)]
                     (str/replace s whole-match replacement)))
           block-str
           matches)
       remove-entry))))

(comment
  (re-seq (re-pattern "\\(\\(\\(?([^)]*)\\)?\\)\\)") "((hello))"))

(defn extract-embeds [block-str]
  (let [uid (second (first (re-seq
                             (re-pattern "\\{\\{\\[\\[embed\\]\\]\\: \\(\\(\\(?([^)]*)\\)?\\)\\)")
                             block-str)))
        c  (when (some? uid)
             (q '[:find (pull ?e [:block/string {:block/children ...}])
                  :in $ ?uid
                  :where
                  [?e :block/uid ?uid]]
               uid))]
    c))

(comment
  (extract-embeds "{{[[embed]]: ((oyU-xMonE))}}"))

(defn move-block [parent-uid order block-uid]
  (-> (j/call-in js/window [:roamAlphaAPI :data :block :move]
        (clj->js {:location {:parent-uid parent-uid
                             :order       order}
                  :block    {:uid    block-uid}}))
    (.then (fn []
             #_(println "new block in messages")))))


(defn create-new-block [parent-uid order string callback]
  #_(println "create new block" parent-uid)
  (-> (j/call-in js/window [:roamAlphaAPI :data :block :create]
        (clj->js {:location {:parent-uid parent-uid
                             :order       order}
                  :block    {:uid    (gen-new-uid)
                             :string string
                             :open   true}}))
    (.then (fn []
             callback))))

(defn update-block-string
  ([block-uid string]
   (update-block-string block-uid string #()))
  ([block-uid string callback]
   (-> (j/call-in js/window [:roamAlphaAPI :data :block :update]
         (clj->js {:block {:uid    block-uid
                           :string string}}))
     (.then (fn [] callback)))))

(defn update-block-string-for-block-with-child [block-uid c1 c2 new-string]
  (let [uid (get-child-of-child-with-str block-uid c1 c2 false)]
    (update-block-string uid new-string)))


(defn update-block-string-and-move [block-uid string parent-uid order]
  (-> (j/call-in js/window [:roamAlphaAPI :data :block :update]
        (clj->js {:block {:uid    block-uid
                          :string string}}))
    (.then (fn []
             #_(println "-- updated block string now moving --")
             (move-block parent-uid order block-uid)))))


(defn create-new-block-with-id [{:keys [parent-uid block-uid order string callback open]}]
  (-> (j/call-in js/window [:roamAlphaAPI :data :block :create]
        (clj->js {:location {:parent-uid parent-uid
                             :order       order}
                  :block    {:uid    block-uid
                             :string string
                             :open   open}}))
    (.then (fn []
             callback))))

(defn create-new-page
  ([title]
   (let [uid (gen-new-uid)]
     (create-new-page title uid)))
  ([title uid]
   (-> (j/call-in js/window [:roamAlphaAPI :data :page :create]
         (clj->js {:page {:title title
                          :uid   uid}}))
     (.then (fn []
              #_(println "new page created"))))))

;; The keys s - string, c - children, u - uid, op - open, o - order
#_(extract-struct
    {:s "AI chats"
     :c [{:s "{{ chat-llm }}"
          :c [{:s "Messages"}
              {:s "Context"}]}]}
    "8yCGreTXI")
(defn create-struct
  ([struct top-parent chat-block-uid open-in-sidebar?]
   (create-struct struct top-parent chat-block-uid open-in-sidebar? #()))
  ([struct top-parent chat-block-uid open-in-sidebar? cb]
   (let [pre   "*Creating struct*: "
         stack (atom [struct])
         res (atom [top-parent])]
     (p pre struct)
     (p (str pre "open in sidebar?") open-in-sidebar?)
     (go
       (while (not-empty @stack)
          (let [cur                  (first @stack)
                {:keys [t u s o op
                        string
                        title
                        uid
                        order]}      cur
                new-uid              (j/call-in js/window [:roamAlphaAPI :util :generateUID])
                parent               (first @res)
                u                    (or u uid)
                o                    (or o order)
                s                    (or s string)
                t                    (or t title)
                c                    (or (:c cur) (:children cur))
                args                 {:parent-uid parent
                                       :block-uid  (if (some? u ) u new-uid)
                                       :order      (if (some? o) o "last")
                                       :string     s
                                       :open      (if (some? op) op true)}]
              (swap! stack rest)
              (swap! stack #(vec (concat % (sort-by :order c))))
              ;(println "block-" string "-parent-" parent #_(first @res))
              (p (str pre "creating with args: " t  " -- " args))
              (cond
                (some? t) (<p! (create-new-page t (if (some? u) u  new-uid)))
                (some? s) (<p! (create-new-block-with-id args)))
              (swap! res rest)
              (swap! res #(vec (concat % (vec (repeat (count c)
                                                (if (some? u)
                                                  u
                                                  new-uid))))))))
       (when open-in-sidebar?
         (p "open window in sidebar? " open-in-sidebar?)
         (<p! (-> (j/call-in js/window [:roamAlphaAPI :ui :rightSidebar :addWindow]
                    (clj->js {:window {:type "block"
                                       :block-uid chat-block-uid
                                       :order 0}}))
                (.then (fn []
                          (do
                           (p "Window added to right sidebar")
                           (j/call-in js/window [:roamAlphaAPI :ui :rightSidebar :open])))))))

      (<p! (js/Promise. (fn [_]
                          cb)))))))

(def gemini-safety-settings-struct
  {:s "Safety settings"
   :op false
   :c [{:s "Harassment"
        :c [{:s "Block some"}]}
       {:s "Hate Speech"
        :c [{:s "Block some"}]}
       {:s "Sexually Explicit"
        :c [{:s "Block some"}]}
       {:s "Dangerous Content"
        :c [{:s "Block some"}]}]})



(defn get-focused-block []
  (-> (j/call-in js/window [:roamAlphaAPI :ui :getFocusedBlock])
    (j/get :block-uid)))


(defn llm-chat-settings-page-struct []
  (let [page-uid    (gen-new-uid)
        page-title  "LLM chat settings"
        page-exist? (some? (get-eid page-title))]
    (p "*" page-title "* page exists?" page-exist?)
    (when (not page-exist?)
      (p "*" page-title "* page does NOT exist, so creating it")
      (create-struct
        {:t  page-title
         :u page-uid
         :c [{:s "Quick action buttons"
              :c [#_{:s "Summarise this page"
                     :c [{:s "Context"
                          :c [{:s "This is Dr. Akamatsu's biology lab at the University of Washington. Our lab uses Roam Research to organize our collaboration and knowledge sharing related to understanding endocytosis in cells.\n\nWe capture questions (QUE), hypotheses (HYP), results (RES), and sources (which begin with @) on separate pages in Roam. We call these discourse pages. Each page has a title summarizing the key insight, a body detailing relevant content, and hierarchical references (refs) linking to related pages. The refs show the lineage of ideas from one page to detailed explorations on another. By connecting the dots across pages, we maintain an audit trail of the evolution in our research.\n\nFor example, a QUE page may ask \"How does the Arp2/3 complex bind to actin filaments?\" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to RES pages containing observations that support or oppose our hypothesis. The RES page links to a source, which can correspond to a published research article or our ongoing experiment or simulation.\n\nThe provided page data reflects this structure.  Each individual page is a map with keys `:title`, `:body` and `:refs`. The body content combines biology knowledge with our lab's own analyses and experimental data.\n\nGiven the following data from a page summarize it for me as an expert in this field. For your responses, make use of the linked references, and strive to be detailed and accurate. When possible, cite discourse page titles verbatim, surrounded in double square brackets, to ground your responses. \n\nData from page: "}]}]}
                  ;; :c [{:s "This is Dr. Akamatsu's biology lab at the University of Washington. Our lab uses Roam Research to organize our collaboration and knowledge sharing related to understanding endocytosis in cells.\n\nWe capture questions (QUE), hypotheses (HYP), and conclusions (CON) on separate pages in Roam. Each page has a title summarizing the key insight, a body elaborating on findings and literature, and hierarchical references (refs) linking to related pages. The refs show the lineage of ideas from one page to detailed explorations on another.\n\nFor example, a QUE page may ask \"How does the Arp2/3 complex bind to actin filaments?\" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to CON pages concluding whether our hypothesis was supported or refuted.\n\nOur pages integrate knowledge from publications, data visualizations, and discussions with experts in the field. By connecting the dots across pages, we maintain an audit trail of the evolution in our research.\n\nThe provided page data reflects this structure, each individual page is a map with keys `:title`, `:body` and `:refs`. The body content combines biology expertise with our lab's own analyses and experimental data.\n\nGiven the following data from a page summarise it for me as  nexpert in this field. Use the linked references for your answers, go in depth.\n\nData from page: "}]}]}
                  {:s "Settings"
                   :c [{:s "Token count"
                        :c [{:s "0"}]}
                       {:s "Model"
                        :c [{:s "gpt-4o-vision"}]}
                       {:s "Max tokens"
                        :c [{:s "400"}]}
                       {:s "Temperature"
                        :c [{:s "0.9"}]}
                       {:s "Get linked refs"
                        :c [{:s "false"}]}
                       {:s "Extract query pages"
                        :c [{:s "false"}]}
                       {:s "Extract query pages ref?"
                        :c [{:s "false"}]}
                       {:s "Context for discourse graph suggestion"
                        :c [{:s "Based on our previous conversation propose some new discourse nodes.\nNote: \n\n 1. follow the following format, this is format of the following lines `node type - format to follow if the node is of this type`. For each suggestion put it on a newline.\n\n```javascript\n[[CON]] - {content}                // Use format for node type \"Conclusion\"\n[[RES]] - {content} - [[@Source]]  // Use format for node type \"Result\"\n[[HYP]] - {content}                // Use format for node type \"Hypothesis\"\n[[ISS]] - {content}                // Use format for node type \"Issue\"\n@{content}                         // Use format for node type \"Source\"\n[[EVD]] - {content} - [[@Source]]  // Use format for node type \"Evidence\"\n[[QUE]] - {content}                // Use format for node type \"Question\"\n[[CLM]] - {content}                // Use format for node type \"Claim\"```\n\n2. following the format does not mean degrading your answer quality. We want both follow the format and high quality suggestions. \n3. Please only reply with discourse node suggestions, not explanations, keep them high quality. "}]}]}
                  {:s "System prompt"
                   :c [{:s "Pre prompt"
                        :c [{:s "This is Dr. Akamatsu's biology lab at the University of Washington. Our lab uses Roam Research to organize our collaboration and knowledge sharing related to understanding endocytosis in cells.\n\nWe capture questions (QUE), hypotheses (HYP), and conclusions (CON) on separate pages in Roam. Each page has a title summarizing the key insight, a body elaborating on findings and literature, and hierarchical references (refs) linking to related pages. The refs show the lineage of ideas from one page to detailed explorations on another.\n\nFor example, a QUE page may ask \"How does the Arp2/3 complex bind to actin filaments?\" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to CON pages concluding whether our hypothesis was supported or refuted.\n\nOur pages integrate knowledge from publications, data visualizations, and discussions with experts in the field. By connecting the dots across pages, we maintain an audit trail of the evolution in our research.\n\nThe provided page data reflects this structure, each individual page is a map with keys `:title`, `:body` and `:refs`. The body content combines biology expertise with our lab's own analyses and experimental data."}]}]}
                  #_{:s "Image prompt"
                     :c [{:s "Default prompt"
                          :c [{:s "What's in this image?"}]}
                         {:s "Settings"
                          :c [{:s "Max tokens"
                               :c [{:s "300"}]}
                              {:s "Generate description for:"
                               :c [{:s "Images without description"}]}]}]}
                  {:s "Discourse graph this page"
                   :c [{:s "Context"
                        :c [{:s "Our lab uses Roam Research to organize our collaboration and knowledge sharing related to understanding endocytosis in cells.\n\nWe capture questions (QUE), hypotheses (HYP), and conclusions (CON) on separate pages in Roam. Each page has a title summarizing the key insight. We call these discourse nodes.\n\nFor example, \n\n<example> \na QUE page may ask \"How does the Arp2/3 complex bind to actin filaments?\" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to RES pages that either support or oppose the hypothesis.\n</example>\n\n<your-job>\nBased on the text and images provided, propose some new discourse nodes. This should include several new results, and possibly claims or issues. Candidate results are tagged with [[RES]].\n</your-job>\n\n<instructions>\n\n<exactly-follow-our-lab-ontology>\n1. Result (RES) is a research observation. \n1.1 The title of the result needs to be in past tense.\n1.2 should include an observation, model system, and approach. \n1.3 should focus on observation rather than interpretation.\n1.4 should include the source of your result.\n\nExample for instruction 1: <example> [[RES]] - Increasing membrane tension from 150 pN/µm to 1000 pN/µm in endocytosis simulations increased the cumulative antagonistic force on filament ends - [@cytosim/vary force-dependent capping in endocytosis simulation] </example> \n\n2. Hypothesis (HYP) is a testable claim \n2.1 in present tense. \n\n3. Issue (ISS) is a desired experiment, or \"request for experiment\".\n3.1 in the form of a potential analysis, literature search, simulation, or experiment. \n</exactly-follow-our-lab-ontology>\n\n<expected-response-format>\n- follow the following format, this is format of the following lines `node type - format to follow if the node is of this type`. For each suggestion put it on a new line.\n\n[[RES]] - {content} - [@Source]  // Use format for node type \"Result\"\n[[ISS]] - {content}                // Use format for node type \"Issue\"\n[[CLM]] - {content}                // Use format for node type \"Claim\"\n[[HYP]] - {content}                // Use format for node type \"Hypothesis\"\n\n- <Important-note> replace the `Source` with actual source. </important-note>\n</expected-response-format>\n\n<general-important-instructions>\n1. following the format does not mean degrading your answer quality. We want both follow the format and high quality suggestions. Make sure your {content} draws directly from the text and images provided.\n2. Please only reply with discourse node suggestions, not explanations, keep them high quality. \n</general-important-instructions>\n</instructions>\n\nData from page: \n<data-from-page(s)>\n"}]}
                       {:s "Settings"
                        :c [{:s "Model"
                             :c [{:s "gpt-4-vision"}]}
                            {:s "Temperature"
                             :c [{:s "0.3"}]}
                            {:s "Max tokens"
                             :c [{:s "500"}]}
                            {:s "Get linked refs"
                             :c [{:s "true"}]}
                            {:s "Extract query pages"
                             :c [{:s "true"}]}
                            {:s "Extract query pages ref?"
                             :c [{:s "true"}]}]}]}
                  {:s "Get context"
                   :c [{:s "Prompt"
                        :c [{:s "Pre-prompt"
                             :c [{:s "You are an AI research assistant specialising in endocytosis and cell biology. We capture questions (QUE), hypotheses (HYP), and conclusions (CON) on separate pages in Roam. Each page has a title summarising the key insight. We call these discourse nodes. Your task is to analyse and synthesise information from a Roam Research database, providing a comprehensive overview of a specific research topic. Your response should be based on the provided roam page content, and related nodes, supplemented by your expert knowledge.\n\n<input-data-format> \n{title: \"Title of the discourse node\",\n body: \"Content of the page\",\n linked-refs: \"References made from other discourse nodes to this page\"}\n</input-data-format>\n\nNow following you have the real data based on this format:\n"}]}
                            {:s "Further instructions"
                             :c [{:s "Please structure your response as follows:\n\n<summary>\nProvide a concise, high-level overview of the research topic, including:\n1. The main research question or focus\n2. The underlying motivation for this research\n3. The primary hypothesis or hypotheses\n4. Potential significance and implications of the research\n\nBase this summary primarily on the provided discourse node page content's body, supplemented by the linked references content. Use your expertise to offer insights and connections.\n</summary>\n\n<context>\nElaborate on the research context:\n1. Explain how this topic fits within the larger research project or question\n2. Identify key concepts, methods, or findings relevant to this research\n3. Highlight any challenges or open questions in this area of study\n\nExplicitly cite relevant discourse nodes present in <discourse-node-page-content's body and its linked-refs when possible. You may also draw from your knowledge base to provide additional context or speculate on the significance.\n</context>\n<important-instruction> While referencing a node use roam format of double brackets e.g `[[page title]]` <example> [[[[[[ISS]] - this is issue]]]] </example>\n</important-instruction>\nEnsure your response is:\n1. Comprehensive and well-structured\n2. Accurately reflects the provided information\n3. Demonstrates deep understanding of endocytosis and related cell biology concepts\n4. Provides insightful analysis and connections between different aspects of the research\n5. Uses clear, scientific language appropriate for an academic audience"}]}]}
                       {:s "Settings"
                        :c [{:s "Model"
                             :c [{:s "gemini-2.0-flash"}]}
                            {:s "Temperature"
                             :c [{:s "0.2"}]}
                            {:s "Max tokens"
                             :c [{:s "1400"}]}
                            {:s "Get linked refs"
                             :c [{:s "true"}]}
                            {:s "Extract query pages"
                             :c [{:s "false"}]}
                            {:s "Extract query pages ref?"
                             :c [{:s "true"}]}]}]}
                  {:s "Get suggestions"
                   :c [{:s "Prompt"
                        :c [{:s "Pre-prompt"
                             :c [{:s "<system>\nYou are an advanced AI research assistant specializing in optimizing scientific workflows through sophisticated discourse graphs. Your role is to analyze complex research notes, identify existing discourse nodes, and **suggest new ideas** to enhance the research narrative and structure, with a focus on creativity within defined constraints.\n</system>\n\n<input-data-format> \n{title: \"Title of the discourse node\",\n body: \"Content of the page\",\n linked-refs: \"References made from other discourse nodes to this page\"}\n</input-data-format>"}]}
                            {:s "Further instructions"
                             :c [{:s "Our lab uses Roam Research to organize our collaboration and knowledge sharing related to understanding endocytosis in cells.\n\nWe capture questions (QUE), hypotheses (HYP), and conclusions (CON) on separate pages in Roam. Each page has a title summarizing the key insight. We call these discourse nodes.\n\nFor example, \n\n<example> \na QUE page may ask \"How does the Arp2/3 complex bind to actin filaments?\" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to RES pages that either support or oppose the hypothesis.\n</example>\n\n<types-of-discourse-nodes>\nQuestion (QUE): A research inquiry or topic that needs investigation or synthesis.\nClaim (CLM): A statement or assertion that is supported by evidence.\nEvidence (EVD): Information, data, or observations that support or relate to a claim or question.\nHypothesis (HYP): A proposed explanation or prediction that can be tested through experiments or further research.\nResult (RES): The outcome or finding from an experiment or study.\nExperiment (EXP): A scientific procedure undertaken to test a hypothesis or answer a question.\nConclusion (CON): An interpretation or judgment drawn from results or evidence.\nIssue (ISS): A problem, challenge, or topic that requires attention or investigation.\n</types-of-discourse-nodes>\n\n\n<discourse-graph-workflow>\nThe discourse graph workflow is an iterative process that structures scientific inquiry and knowledge building. It begins with an Issue, which leads to the formulation of a Hypothesis. This Hypothesis is then tested through an Experiment, which produces Results. These Results are interpreted to form Claims, which can contribute to broader Models or Theories. Each step naturally progresses to the next, creating a cycle of investigation and discovery. However, the workflow is flexible, allowing movement back to previous steps as new information emerges. This structure captures the complex, non-linear nature of scientific research while providing a clear path for advancement. The workflow encourages researchers to continually push their investigations forward, moving from questions to theories and back again, fostering a systematic approach to scientific understanding.\n</discourse-graph-workflow>\n\n<your-job>\nBased on the text and images provided, first determine where the context seems to fit in <discourse-graph-workflow>, then if it is:\n\nan Issue:\n-> Formulate a Hypothesis\na Hypothesis:\n-> Design an Experiment to test it\nan Experiment:\n-> Identify or prompt for Results (observations)\nIa Result:\n-> Tie it to a Claim, interpreting its significance\na Claim:\n-> Propose new Issues (possible experiments) to further investigate or validate the claim\n-> Consider how it might contribute to or modify existing Models/Theories\na Model/Theory:\n-> Generate new Issues or Hypotheses to test or refine the model\n\nThe goal is to guide the user to next steps based on the current context. The user expects answers to question like \"point me in next direction\" \"suggest next steps\". They are looking for a detailed creative response that is firmly grounded in the provided context.\n\n</your-job>\n\n<general-important-instructions>\nfollowing the format does not mean degrading your answer quality. We want both follow the format and high quality suggestions. Make sure your {content} draws directly from the text and images provided.\nPlease use the linked-refs data also for your context. Take everything in context and provide a creative based answer. \n</general-important-instructions>\n\n\n"}]}]}
                       {:s "Settings"
                        :c [{:s "Model"
                             :c [{:s "gpt-4-vision"}]}
                            {:s "Temperature"
                             :c [{:s "0.9"}]}
                            {:s "Max tokens"
                             :c [{:s "2000"}]}
                            {:s "Get linked refs"
                             :c [{:s "true"}]}
                            {:s "Extract query pages"
                             :c [{:s "false"}]}
                            {:s "Extract query pages ref?"
                             :c [{:s "true"}]}]}]}]}]}
        page-uid
        nil
        nil
        (p "*" page-title "* page Created")))))




(def settings-struct
  {:s "Settings"
   :op false
   :c [{:s "Token count"
        :c [{:s "0"}]}
       {:s "Model"
        :c [{:s "gpt-4o-mini"}]}
       {:s "Max tokens"
        :c [{:s "400"}]}
       {:s "Temperature"
        :c [{:s "0.9"}]}
       {:s "Get linked refs"
        :c [{:s "true"}]}
       {:s "Active?"
        :c [{:s "false"}]}
       {:s "Extract query pages"
        :c [{:s "true"}]}
       {:s "Extract query pages ref?"
        :c [{:s "true"}]}]})



(defn common-chat-struct [context-structure context-block-uid context-open?]
  [{:s "Messages"}
   {:s "Context"
    :op (or context-open? true)
    :c (or context-structure [{:s ""}])
    :u (or context-block-uid nil)}
   {:s "Chat"
    :c [{:s ""}]}
   settings-struct])


(defn default-chat-struct
  ([]
   (default-chat-struct nil nil nil))
  ([chat-block-uid]
   (default-chat-struct chat-block-uid nil nil))
  ([chat-block-uid context-block-uid]
   (default-chat-struct chat-block-uid context-block-uid nil))
  ([chat-block-uid context-block-uid chat-block-order]
   (default-chat-struct chat-block-uid context-block-uid chat-block-order nil))
  ([chat-block-uid context-block-uid chat-block-order context-structure]
   {:s "{{ chat-llm }}"
    :op false
    :o chat-block-order
    :u (or chat-block-uid nil)
    :c (common-chat-struct context-structure context-block-uid false)}))


(defn chat-ui-with-context-struct
  ([]
   (chat-ui-with-context-struct nil nil nil))
  ([chat-block-uid]
   (chat-ui-with-context-struct chat-block-uid nil nil))
  ([chat-block-uid context-block-uid]
   (chat-ui-with-context-struct chat-block-uid context-block-uid nil))
  ([chat-block-uid context-block-uid  context-structure]
   (chat-ui-with-context-struct chat-block-uid context-block-uid context-structure nil))
  ([chat-block-uid context-block-uid  context-structure chat-block-order]
   {:s "AI chats"
    :o chat-block-order
    :op false
    :c [{:s "{{ chat-llm }}"
         :op false
         :u (or chat-block-uid nil)
         :c (common-chat-struct context-structure context-block-uid false)}]}))


;; ---- ai specific ----

(def model-mappings
  {"gpt-4o"            "gpt-4o-2024-08-06"
   "gpt-4-vision"      "gpt-4o-2024-08-06"
   "gpt-4o-mini"       "gpt-4o-mini-2024-07-18"
   "o1"                "o1-preview"
   "o1-mini"           "o1-mini-2024-09-12"
   "claude-3.5-sonnet" "claude-3-5-sonnet-latest"
   "claude-3.5-haiku"  "claude-3-5-haiku-latest"
   "claude-3-haiku"    "claude-3-haiku-20240307"
   "gemini-2.0-flash"  "gemini-2.0-flash-exp"
   "gemini-exp"        "gemini-exp-1206"})


(defn model-type [model-name]
  (cond
    (str/starts-with? model-name "gpt")    :gpt
    (str/starts-with? model-name "o1")     :o1
    (str/starts-with? model-name "claude") :claude
    (str/starts-with? model-name "gemini") :gemini
    :else                                  :unknown))


(def safety-map
  {"Block none"  "BLOCK_NONE"
   "Block few"   "BLOCK_ONLY_HIGH"
   "Block some" "BLOCK_MEDIUM_AND_ABOVE"
   "Block most" "BLOCK_LOW_AND_ABOVE"})


(defn get-safety-settings [chat-block-uid]
  (let [settings-uid      (:uid (get-child-with-str chat-block-uid "Settings"))
        harassment        (get-child-of-child-with-str settings-uid "Safety settings" "Harassment")
        hate-speech       (get-child-of-child-with-str settings-uid "Safety settings" "Hate Speech")
        sexually-explicit (get-child-of-child-with-str settings-uid "Safety settings" "Sexually Explicit")
        dangerous-content (get-child-of-child-with-str settings-uid "Safety settings" "Dangerous Content")]
    (p "safety settings" harassment hate-speech sexually-explicit dangerous-content)
    [{:category "HARM_CATEGORY_HARASSMENT"
      :threshold (get safety-map harassment)}
     {:category "HARM_CATEGORY_HATE_SPEECH"
      :threshold (get safety-map hate-speech)}
     {:category "HARM_CATEGORY_SEXUALLY_EXPLICIT"
      :threshold (get safety-map sexually-explicit)}
     {:category "HARM_CATEGORY_DANGEROUS_CONTENT"
      :threshold (get safety-map dangerous-content)}]))


(goog-define url-endpoint "")

(defn call-api [url messages settings callback]
  (let [passphrase (j/get-in js/window [:localStorage :passphrase])
        data    (clj->js {:documents messages
                          :settings settings
                          :passphrase passphrase})
        headers {"Content-Type" "application/json"}
        res-ch  (http/post url {:with-credentials? false
                                :headers headers
                                :json-params data})]
    (take! res-ch callback)))

(defn call-llm-api [{:keys [messages settings callback]}]
  (println "SETTINGS" settings)
  (let [model (model-type (:model settings))
        new-settings {:model (:model settings)}]
    (println "MODEL NAME" model)
    (case model
      :o1         (call-api "https://roam-llm-chat-falling-haze-86.fly.dev/chat-complete"
                    messages new-settings callback)
      :gpt        (call-api "https://roam-llm-chat-falling-haze-86.fly.dev/chat-complete"
                    messages settings callback)
      :claude     (call-api "https://roam-llm-chat-falling-haze-86.fly.dev/chat-anthropic"
                    messages settings callback)
     :gemini     (call-api "https://roam-llm-chat-falling-haze-86.fly.dev/chat-gemini"
                   messages settings callback)
      (p "Unknown model"))))


(defn count-tokens-api [{:keys [model message token-count-atom update? block-uid]}]
  (p "Count tokens api called")
  (let [url    "https://roam-llm-chat-falling-haze-86.fly.dev/count-tokens"
        data    (clj->js {:model "gpt-4"
                          :message message})
        headers {"Content-Type" "application/json"}
        res-ch  (http/post url {:with-credentials? false
                                :headers headers
                                :json-params data})]
    (take! res-ch (fn [res]
                    (let [count (-> res
                                   :body)
                          new-count (if update?
                                      (+ (js/parseInt  @token-count-atom)
                                         (js/parseInt count))
                                      count)]
                      (p "*Old Token count* :" @token-count-atom)
                      (update-block-string-for-block-with-child block-uid "Settings" "Token count" (str new-count))
                      (reset! token-count-atom new-count)
                      (p "*New Token count* :" new-count))))))

(get-child-of-child-with-str-on-page "LLM chat settings" "Quick action buttons" "System prompt" "Pre prompt")

(defn create-alternate-messages [messages initial-context pre]
  (let [pre-prompt          (str (-> (get-child-of-child-with-str-on-page "LLM chat settings" "Quick action buttons" "System prompt" "Pre prompt")
                                   :children
                                   first
                                   :string))
        context-type-string? (string? initial-context)
        current-message      (if (and (not-empty initial-context) context-type-string?)
                               (atom (str pre-prompt "\n Initial context: \n " (extract-from-code-block initial-context)))
                               (atom ""))
        alternate-messages   (if (and (not-empty initial-context) context-type-string?)
                               (atom [])
                               (atom [{:role "user"
                                       :content (vec (concat
                                                       [{:type "text"
                                                         :text (str pre-prompt "\n Initial context: \n")}]
                                                       (vec initial-context)))}]))]
    (p (str pre "create alternate messages"))
    (p @alternate-messages)
    (doseq [msg messages]
      (let [msg-str (replace-block-uids (:string msg))]
        (if (str/starts-with? msg-str "**Assistant:** ")
          (do
            (swap! alternate-messages conj {:role    "user"
                                            :content (str (clojure.string/replace @current-message #"^\*\*User:\*\* " ""))})
            (swap! alternate-messages conj {:role    "assistant"
                                            :content (str (clojure.string/replace msg-str #"^\*\*Assistant:\*\* " ""))})
            (reset! current-message ""))
          (swap! current-message #(str % "\n" (extract-from-code-block (clojure.string/replace msg-str #"^\*\*User:\*\* " "")))))))
    (when (not-empty @current-message)
      (swap! alternate-messages conj {:role    "user"
                                      :content (str @current-message)}))
    @alternate-messages))



(defn image-to-text-for [nodes total-images-atom loading-atom image-prompt-str max-tokens]
  (js/Promise.
    (fn [resolve _]
      (doseq [node nodes]
        (let [url      (-> node :match :url)
              uid      (-> node :uid)
              text     (-> node :match :text)
              block-string (-> node :string)
              messages [{:role "user"
                         :content [{:type "text"
                                    :text (str image-prompt-str)}
                                   {:type "image_url"
                                    :image_url {:url url}}]}]
              settings  {:model       "gpt-4-vision-preview"
                         :max-tokens  @max-tokens
                         :temperature 0.9}
              callback (fn [response]
                          (let [res-str          (-> response
                                                   :body)
                                new-url          (str "![" res-str "](" url ")")
                                new-block-string (str/replace block-string markdown-image-pattern new-url)
                                new-count        (dec @total-images-atom)]
                            (p "image to text for uid" uid)
                            (p "Old image description: " text)
                            (p "old block string" block-string)
                            (p "Image to text response received, new image description: " res-str)
                            (p "New block string" new-block-string)
                            (p "Images to describe count: " @total-images-atom)
                            (if (zero? new-count)
                              (do
                                (p "All images described")
                                (reset! loading-atom false)
                                (resolve true))
                              (swap! total-images-atom dec))
                            (update-block-string
                              uid
                              new-block-string)))]
          (p "Messages for image to text" messages)
          (call-llm-api
            {:messages messages
             :settings settings
             :callback callback}))))))


;; ---- UI ---


(defn inject-style []
  (let [style-element (.createElement js/document "style")
        css-string ".sp svg { color: cadetblue; }
        .bp3-tooltip {max-width: 200px;}"] ; Change 'blue' to your desired color

    (set! (.-type style-element) "text/css")
    (when (.-styleSheet style-element) ; For IE8 and below.
      (set! (.-cssText (.-styleSheet style-element)) css-string))
    (when-not (.-styleSheet style-element) ; For modern browsers.
      (let [text-node (.createTextNode js/document css-string)]
        (.appendChild style-element text-node)))
    (.appendChild (.-head js/document) style-element)))


(defn send-message-component [active? callback]
  (inject-style)
  [:> Button {:class-name "sp"
              :style {:width "30px"}
              :icon (if @active? nil "send-message")
              :minimal true
              :fill false
              :small true
              :loading @active?
              :on-click #(do #_(println "clicked send message compt")
                           (callback {}))}])

(defn button-popover
  ([button-text render-comp]
   (button-popover button-text render-comp "#eeebeb"))
  ([button-text render-comp bg-color]
   [:> Popover
    {:position "bottom"}
    [:> Button {:minimal true
                :small true}
     button-text]
    [:> Menu
     ;{:style {:padding "20px"}}
     render-comp]]))

(defn settings-button-popover
  ([render-comp]
   (settings-button-popover render-comp "#eeebeb"))
  ([render-comp bg-color]
   [:> Popover
    ;{:position "bottom"}
    [:> Button {:icon "cog"
                :minimal true
                :small true
                :style {:background-color bg-color}}]
    [:> Menu
     {:style {:padding "20px"}}
     [:div
       {:class-name "Classes.POPOVER_DISMISS_OVERRIDE"}
       render-comp]]]))

(defn button-with-tooltip
  ([tooltip-text button-comp]
   (button-with-tooltip tooltip-text button-comp "auto"))
  ([tooltip-text button-comp position]
   (inject-style)
   [:> Popover
    {:position "bottom"}
    [:> Tooltip
     {:content tooltip-text
      :position position
      :openOnTargetFocus false
      :hoverOpenDelay 500}
     button-comp]]))




(defn buttons-settings [button-name
                        block-uid
                        default-temp
                        default-model
                        default-max-tokens
                        get-linked-refs?
                        extract-query-pages?
                        extract-query-pages-ref?]
  (let [dismiss-popover? false]
     (fn [_]
       [:<>
        [:> MenuDivider {:title button-name}]
        [:> MenuItem
         {:text  "Model"
          :label (str @default-model)}
         #_[button-with-tooltip
            "Pricing is per million token but it does not mean we can use 1 Million tokens with every llm.
      Only Gemini models support 1M tokens, Claude supports 200k, GPT-4o supports 128k and GPT-3.5 supports 16k tokens.
      One useful strategy to optimise for pricing is to use different models for different type of messages in the same
      chat for e.g you can use lower priced models to extract the necessary data from the context and then select
      a stronger model to do reasoning on it."]
         [:div
          [button-with-tooltip
           "If your page or context has any images and you want the llm to also consider them while
        replying then you should use the GPT-4 Vision model."
           [:> MenuDivider {:title "With vision capabilities"}]]

          [:> MenuItem
           {:text "GPT-4o"
            :labelElement "$2.50"
            :should-dismiss-popover dismiss-popover?
            :on-click (fn [e]
                        #_(js/console.log "clicked menu item" e)
                        (p "chose gpt-4-vision")
                        (update-block-string-for-block-with-child block-uid "Settings" "Model" "gpt-4-vision")
                        (reset! default-model "gpt-4-vision"))}]
          [button-with-tooltip
           "Gemini-2.0 Flash has 1 Million token context window so if you want to select a few pages with large context
        and then chat with them, this model is the only one able to support such large context and not charge for it.
        The catch is that they might use the data for training their models."
           [:> MenuDivider {:title "Fast, Free, Best, 1M tokens"}]]
          [:> MenuItem
           {:text "Gemini 2.0 Flash"
            :labelElement "$0.00"
            :should-dismiss-popover dismiss-popover?
            :on-click (fn [e]
                        #_(js/console.log "clicked menu item" e)
                        (p "chose gemini-2.0-flash")
                        (update-block-string-for-block-with-child block-uid "Settings" "Model" "gemini-2.0-flash")
                        (reset! default-model "gemini-2.0-flash"))}]
          [:> Menu.Item
           {:text "Gemini Experimental"
            :labelElement "$0.00"
            :should-dismiss-popover dismiss-popover?
            :on-click (fn [e]
                        #_(js/console.log "clicked menu item" e)
                        (p "chose gemini-exp")
                        (update-block-string-for-block-with-child block-uid "Settings" "Model" "gemini-exp")
                        (reset! default-model "gemini-exp"))}]
          [button-with-tooltip
           "For text only context you can consider the reasoning ability, context length and pricing of the models.
        Note that if your context is say 50k tokens and you do back-and-forth with the llm 10 times then your
        total token usage would be approx. 500k (approx. 50k tokens send to llm 10 times)"
           [:> MenuDivider {:title "Top of the line"}]]


          [:> Menu.Item
           {:text "o1"
            :labelElement "$15.0"
            :should-dismiss-popover dismiss-popover?
            :on-click (fn [e]
                        #_(js/console.log "clicked menu item" e)
                        (p "chose o1")
                        (update-block-string-for-block-with-child block-uid "Settings" "Model" "o1")
                        (reset! default-model "o1"))}]
          [:> Menu.Item
            {:text "GPT-4o"
             :labelElement "$2.50"
             :should-dismiss-popover dismiss-popover?
             :on-click (fn [e]
                         #_(js/console.log "clicked menu item" e)
                         (p "chose gpt-4o")
                         (update-block-string-for-block-with-child block-uid "Settings" "Model" "gpt-4o")
                         (reset! default-model "gpt-4o"))}]
          [:> Menu.Item
            {:text "o1-mini"
             :labelElement "$3.00"
             :should-dismiss-popover dismiss-popover?
             :on-click (fn [e]
                         #_(js/console.log "clicked menu item" e)
                         (p "chose o1-mini")
                         (update-block-string-for-block-with-child block-uid "Settings" "Model" "o1-mini")
                         (reset! default-model "o1-mini"))}]
          [:> Menu.Item
            {:text "Claude 3.5 Sonnet"
             :labelElement "$3.00"
             :should-dismiss-popover dismiss-popover?
             :on-click (fn [e]
                         #_(js/console.log "clicked menu item" e)
                         (p "chose claude-3.5-sonnet")
                         (update-block-string-for-block-with-child block-uid "Settings" "Model" "claude-3.5-sonnet")
                         (reset! default-model "claude-3.5-sonnet"))}]
          [button-with-tooltip
           "If you want to some tasks that don't need \"strong\" reasoning ability then you can use these models."
           [:> MenuDivider {:title "Fast and cheap"}]]
          [:> Menu.Item
           {:text "Claude 3.5 Haiku"
            :labelElement "$0.80"
            :should-dismiss-popover dismiss-popover?
            :on-click (fn [e]
                        #_(js/console.log "clicked menu item" e)
                        (p "chose claude-3-haiku")
                        (update-block-string-for-block-with-child block-uid "Settings" "Model" "claude-3.5-haiku")
                        (reset! default-model "claude-3.5-haiku"))}]
          [:> Menu.Item
           {:text "GPT-4o-mini"
            :labelElement "$0.15"
            :should-dismiss-popover dismiss-popover?
            :on-click (fn [e]
                        #_(js/console.log "clicked menu item" e)
                        (p "chose gpt-4o-mini")
                        (update-block-string-for-block-with-child block-uid "Settings" "Model" "gpt-4o-mini")
                        (reset! default-model "gpt-4o-mini"))}]
          [:> Menu.Item
           {:text "Claude 3 Haiku"
            :labelElement "$0.25"
            :should-dismiss-popover dismiss-popover?
            :on-click (fn [e]
                        #_(js/console.log "clicked menu item" e)
                        (p "chose claude-3-haiku")
                        (update-block-string-for-block-with-child block-uid "Settings" "Model" "claude-3-haiku")
                        (reset! default-model "claude-3-haiku"))}]]]

        [:> MenuItem
         {:text "Max output tokens"
          :label (str @default-max-tokens)}
         [button-with-tooltip
          "The maximum number of tokens to generate before stopping. Models may stop before reaching this maximum. This parameter only specifies the absolute maximum number of tokens to generate."
          [:div
           [:span {:style {:margin-bottom "5px"}} "Max output length:"]
           [:> Slider {:min 0
                       :max 4096
                       :step-size (clj->js 50)
                       :label-renderer @default-max-tokens
                       :value @default-max-tokens
                       :label-values [0 4096]
                       :on-change (fn [e]
                                    (update-block-string-for-block-with-child block-uid "Settings" "Max tokens" (str e))
                                    (reset! default-max-tokens e))
                       :on-release (fn [e]
                                     #_(log "slider value" e)
                                     (update-block-string-for-block-with-child block-uid "Settings" "Max tokens" (str e))
                                     (reset! default-max-tokens e))}]]]]
        [:> MenuItem
         {:text "Temperature"
          :label (str @default-temp)}
         [button-with-tooltip
          "Amount of randomness injected into the response.\n\nDefaults to 0.9. Ranges from 0.0 to 1.0 for claude and 0.0 to 2.0 for other models. Use temperature closer to 0.0 for analytical / multiple choice, and closer to 1.0 for creative and generative tasks.\n\nNote that even with temperature of 0.0, the results will not be fully deterministic."
          [:div
           [:span {:style {:margin-bottom "5px"}} "Temperature:"]
           [:> Slider {:min 0
                       :max (if (contains? #{"claude-3-haiku""claude-3.5-sonnet""claude-3-opus"} @default-model)
                              1
                              2)
                       :step-size (clj->js 0.1)
                       :label-renderer @default-temp
                       :value @default-temp
                       :label-values (if (contains? #{"claude-3-haiku""claude-3.5-sonnet""claude-3-opus"} @default-model)
                                       [0 1]
                                       [0 2])
                       :on-change (fn [e]
                                    (let [fe (js/parseFloat (.toFixed e 2))]
                                      (println "E" fe (type fe))
                                      (update-block-string-for-block-with-child block-uid "Settings" "Temperature" (str fe))
                                      (reset! default-temp fe)))
                       :on-release (fn [e]
                                     (let [fe (js/parseFloat (.toFixed e 2))]
                                       (println "E" fe)
                                       (update-block-string-for-block-with-child block-uid "Settings" "Temperature" (str fe))
                                       (reset! default-temp fe)))}]]]]
        [:> MenuItem
         {:text "Get linked refs?"
          :should-dismiss-popover false
          :label-element
          (r/as-element
            [button-with-tooltip
             "For each page in the context, if you want to include references to other discourse nodes from that page select this option."
             [:> Checkbox
              {:style {:margin-bottom "0px"}
               :checked @get-linked-refs?
               :on-change (fn [x]
                            (update-block-string-for-block-with-child block-uid "Settings" "Get linked refs" (str (not @get-linked-refs?)))
                            (reset! get-linked-refs? (not @get-linked-refs?)))}]])}]

        [:> MenuItem
         {:text "Extract query pages?"
          :should-dismiss-popover false
          :labelElement
          (r/as-element
            [button-with-tooltip
             "When checked, if you put any page in the context, and that page has a query block in it then we will extract
            all the results of the query block. For each result if the result is a page then we extract the whole content of
            the result page and if the result is a block ref we extract the content of the block ref. "
             [:> Checkbox
              {:style {:margin-bottom "0px"}
               :checked @extract-query-pages?
               :on-change (fn [x]
                            (update-block-string-for-block-with-child block-uid "Settings" "Extract query pages" (str (not @extract-query-pages?)))
                            (reset! extract-query-pages? (not @extract-query-pages?)))}]])}]

        [:> MenuItem
         {:text "Extract query pages refs?"
          :should-dismiss-popover false
          :labelElement
          (r/as-element
            [button-with-tooltip
             "This builds on top of the previous button `Extract query results?`. For each page result in the query builder's list of results
         we also extract that result page's linked discourse node references. "
             [:> Checkbox
              {:style {:margin-bottom "0px"}
               :checked @extract-query-pages-ref?
               :on-change (fn [x]
                            (update-block-string-for-block-with-child block-uid "Settings" "Extract query pages ref?" (str (not @extract-query-pages-ref?)))
                            (reset! extract-query-pages-ref? (not @extract-query-pages-ref?)))}]])}]])))

(defn pull-deep-block-data [uid]
  (q '[:find (pull ?e  [:block/string :block/uid {:block/children ...}])
       :in $ ?e]
    (uid->eid uid)))

(defn index-by-string
  "Given a list of blocks, return a map keyed by the block's :string."
  [blocks]
  (reduce (fn [acc b]
            (assoc acc (:string b) b))
    {}
    blocks))

(defn get-first-child-string
  "Given a block that has at least one child with :string,
   return that child's string value."
  [block]
  (-> block :children first :string))

(defn extract-data [data]
  (let [root          (-> data first first)
        top-level-map (index-by-string (:children root))
        settings-block (top-level-map "Settings")
        prompt-block   (top-level-map "Prompt")
        settings-map  (index-by-string (:children settings-block))
        prompt-map  (index-by-string (:children prompt-block))
        ;; Helper to get a setting's value
        get-setting    (fn [key] (get-first-child-string (settings-map key)))
        get-prompt    (fn [key] (get-first-child-string (prompt-map key)))]

    ;; Construct the final data structure:
    {:messages               (top-level-map "Messages")
     :context                (top-level-map "Context")
     :chat                   (top-level-map "Chat")
     :token-count            (js/parseInt (get-setting "Token count"))
     :model                  (get-setting "Model")
     :max-tokens             (js/parseInt (get-setting "Max tokens"))
     :pre-prompt             (get-prompt "Pre-prompt")
     :further-instructions   (get-prompt "Further instructions")
     :temperature            (js/parseFloat (get-setting "Temperature"))
     :get-linked-refs?       (= "true" (get-setting "Get linked refs"))
     :active?                (= "true" (get-setting "Active?"))
     :extract-query-pages?   (= "true" (get-setting "Extract query pages"))
     :extract-query-pages-ref? (= "true" (get-setting "Extract query pages ref?"))}))

(comment
  (-> "PX0i1rCnq"
    ;"YzeIVbeEy"
    (pull-deep-block-data)
    extract-data
    :context
    :uid))
