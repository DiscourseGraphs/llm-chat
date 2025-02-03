(ns ui.extract-data.chat
  (:require
    [applied-science.js-interop :as j]
    [reagent.core :as r]
    [ui.utils :as utils :refer [is-a-page? uid->title title->uid markdown-image-pattern p extract-embeds uid->eid replace-block-uids q uid-to-block get-eid]]
    [clojure.string :as str]))

(def skip-blocks-with-string #{"{{ chat-llm }}"
                               "AI chats"
                               "AI summary"
                               "AI: Get context"
                               "AI: Get suggestions for next steps"
                               "AI Discourse node suggestions"
                               "AI: Discourse node suggestions"
                               "AI: Prior work"
                               "{{llm-dg-suggestions}}"
                               "Can't let it go"})

(comment
  (contains? skip-blocks-with-string "{{ chat-llm }}"))


(defn extract-markdown-image [s]
  (let [matches (re-find markdown-image-pattern s)]
    (if matches
      {:text (nth matches 1)
       :url (nth matches 2)}
      false)))

(comment
  (extract-markdown-image "![image](https://www.google.com)")
  (extract-markdown-image "![image](https://www.google.com) text at the end")
  (extract-markdown-image "text at the start ![image](https://www.google.com) text at the end")
  (str/replace "text at the start ![image](https://www.google.com) text at the end" markdown-image-pattern "gm")
  (extract-markdown-image "![The image shows a bar graph labeled \"B\". The title of the graph is \"Branch angle distribution (cryo-ET)\" which indicates that it's likely displaying data acquired through cryo-electron tomography (cryo-ET), a technique used for structural analysis typically in a biological context.\n\nThe x-axis represents branch angle in degrees, ranging from 0 to just under 90 degrees. The y-axis represents the number of branches, with counts ranging from 0 to 25. The bars represent the frequency distribution of branch angles with most of the data concentrated around a branch angle between 60 and 75 degrees, as indicated by the taller bars in that range.\n\nThe graph also includes a label indicating the mean branch angle: \"Mean: 68 ± 9°\" which suggests the average branch angle is 68 degrees with a standard deviation of 9 degrees.](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fresultsgraph%2FOFNSuhLiaa.png?alt=media&token=88eca532-776c-4414-bcc3-62387cc55dd5)"))

(defn find-blocks-with-images [node no-description?]
  (when-not (contains? skip-blocks-with-string (:string node))
     (let [block?          (some? (:string node))
           current-image? (if block? (extract-markdown-image (:string node)) false)
           without-desc   (empty? (:text current-image?))
           uid            (:uid node)
           children       (or (:children node)
                            [])]
       (concat
         (if no-description?
           (if (and current-image? without-desc)
             [{:uid uid
               :string (:string node)
               :match current-image?}]
             [])
           (if current-image?
             [{:uid uid
               :string (:string node)
               :match current-image?}]
             []))
         (mapcat #(find-blocks-with-images % no-description?) children)))))



(defn get-all-images-for-node
  ([node block?]
   (get-all-images-for-node node block? false))
  ([node block? with-no-description-only?]
   (let [no-description?    (= "Images without description" with-no-description-only?)
         eid                (cond
                              (int? node) node
                              block?   (uid->eid node)
                              :else    (get-eid node))
         children           (ffirst (q '[:find (pull ?eid [{:block/children ...} :block/string :block/uid])
                                         :in $ ?eid]
                                      eid))
         blocks-with-images (find-blocks-with-images children no-description?)]
     blocks-with-images)))

(comment
  (get-all-images-for-node "Test: Image to text" false true)
  (get-all-images-for-node "testing 3" false)
  (get-all-images-for-node "YNWR7PAne" true)
  (get-all-images-for-node "Qr3-5Zogr" #_"YNWR7PAne" true)
  (get-all-images-for-node "[[EVD]] - Cryo electron tomography of actin branches in SKMEL2 cells showed a branching angle of 68 ± 15 degrees - @serwas2022mechanistic" false)
  (get-all-images-for-node "[[EVD]] - NWASP was found around clusters of clathrin heavy chain by TIRF microscopy + super resolution microscopy - [[@leyton-puig2017flat]]" false)
  (get-all-images-for-node "[[QUE]] - What is the rate at which cofilin binds actin filaments?" false))


(defn extract-all-data [title]
  (ffirst (q '[:find (pull ?eid [{:block/children ...} :block/string :block/order :block/uid])
               :in $ ?eid]
            (get-eid title))))


(defn sort-children [node]
  (if (contains? node :children)
    (assoc node :children (vec (->> (:children node)
                                 (map sort-children)
                                 (sort-by :order))))
    node))


(defn extract-strings [{:keys [tree extract-query-pages? only-pages? vision?]}]
  (let [result (atom [])
        stack (atom [tree])]
      (while (not (empty? @stack))
        (let [node             (peek @stack)
              string           (replace-block-uids (:string node))
              block-ref?       (when string (re-seq (re-pattern "\\(\\(\\(?([^)]*)\\)?\\)\\)") (:string node)))
              query-block?     (= string "{{query block}}")
              refed-qry-block? (and query-block?
                                 (some? block-ref?))
              node-uid         (if refed-qry-block?
                                 (second (first block-ref?))
                                 (:uid node))
              current-image?   (if string (extract-markdown-image string) false)
              embed?           (when (some? string)
                                 (ffirst (not-empty (extract-embeds (:string node)))))
              children         (atom (:children node))]
          (swap! stack pop)
          (when (and string
                  (not (contains? skip-blocks-with-string string)))
           ; (p "string" string node-uid)
            (cond
              (and query-block?
                extract-query-pages?)      (let [query-result (-> (j/call-in js/window [:roamjs :extension :queryBuilder :runQuerySync] node-uid)
                                                                (js->clj :keywordize-keys true))]
                                             (p "query-result" query-result)
                                             (doseq [n query-result]
                                               (p " query res: "(:uid n) (:text n))
                                               (let [nstr  (:text n)
                                                     page? (some? (uid->title (:uid n)))]
                                                (cond
                                                  (not page?) (swap! result conj (replace-block-uids nstr))
                                                  only-pages? (swap! result conj (str "[[" nstr "]]"))
                                                  :else       (let [ext [(-> (extract-all-data nstr)
                                                                           (sort-children))]]
                                                                (swap! stack conj (first ext)))))))
              embed?                       (do
                                             (swap! result conj (:string embed?))
                                             (swap! children concat (:children embed?)))
              (and (not vision?)
                current-image?)            (swap! result conj (:text current-image?))
              :else                        (swap! result conj string)))
          (doseq [child (reverse (sort-by :order @children))]
            (when (and
                    (not query-block?)
                    (not (contains? skip-blocks-with-string string)))
              (swap! stack conj child)))))
    ;(p "result" @result)
    @result))


(defn extract-strings-with-level [{:keys [tree extract-query-pages? only-pages? vision? level-threshold]}]
  (let [result (atom [])
        stack (atom [[tree 0]])] ; Track depth with each node
    (while (not (empty? @stack))
      (let [[node depth] (peek @stack)
            string (replace-block-uids (:string node))
            block-ref? (when string (re-seq (re-pattern "\\(\\(\\(?([^)]*)\\)?\\)\\)") (:string node)))
            query-block? (= string "{{query block}}")
            refed-qry-block? (and query-block?
                               (some? block-ref?))
            node-uid (if refed-qry-block?
                       (second (first block-ref?))
                       (:uid node))
            current-image? (if string (extract-markdown-image string) false)
            embed? (when (some? string)
                     (ffirst (not-empty (extract-embeds (:string node)))))
            children (atom (:children node))]
        (swap! stack pop)
        (when (and string
                (not (contains? skip-blocks-with-string string)))
          (if (< depth level-threshold)
            ; For levels below threshold, include both UID and string
            (swap! result conj {:uid node-uid :string string})
            ; For level at threshold and deeper, previous behavior
            (cond
              (and query-block?
                extract-query-pages?) (let [query-result (-> (j/call-in js/window [:roamjs :extension :queryBuilder :runQuerySync] node-uid)
                                                           (js->clj :keywordize-keys true))]
                                        (doseq [n query-result]
                                          (let [nstr (:text n)
                                                page? (some? (uid->title (:uid n)))]
                                            (cond
                                              (not page?) (swap! result conj (replace-block-uids nstr))
                                              only-pages? (swap! result conj (str "[[" nstr "]]"))
                                              :else (let [ext [(-> (extract-all-data nstr)
                                                                 (sort-children))]]
                                                      (swap! stack conj [(first ext) (inc depth)]))))))
              embed?                  (do
                                        (swap! result conj (:string embed?))
                                        (swap! children concat (:children embed?)))
              (and (not vision?)
                current-image?)       (swap! result conj (:text current-image?))
              :else                   (swap! result conj string))))
        ; Add children to stack with incremented depth
        (doseq [child (reverse (sort-by :order @children))]
          (when (and
                  (not query-block?)
                  (not (contains? skip-blocks-with-string string)))
            (swap! stack conj [child (inc depth)])))))
    @result))

(defn get-children-for [{:keys [node block? extract-query-pages? only-pages? vision?]}]
   (p "Inside get children for particular page or block function: " node)
   (when-let [eid   (cond
                      (int? node) node
                      block?   (uid->eid node)
                      :else    (get-eid node))]
        (let [raw-children-data (ffirst (q '[:find (pull ?eid [{:block/children ...} :block/string :block/order :block/uid])
                                             :in $ ?eid]
                                          eid))
              embed?             (when block? (ffirst (not-empty (extract-embeds (:string raw-children-data)))))
              sorted-by-children (sort-children raw-children-data)
              extracted-strings  (extract-strings {:tree                 (if embed? embed? sorted-by-children)
                                                   :extract-query-pages? (or extract-query-pages? false)
                                                   :only-pages?          only-pages?
                                                   :vision?              vision?})]
          (if only-pages?
            {:extracted-query-pages extracted-strings}
            {:plain-text extracted-strings}))))


(comment

  (get-children-for {:node "testing 5"})

  (get-children-for {:node "Test: Image to text"
                     :only-pages? true})


  (get-children-for {:node "Test: extract query pages"
                     :extract-query-pages? false
                     :only-pages? false
                     :get-linked-refs? true})

  (get-children-for {:node "Testing" ;"Qr3-5Zogr"   ; "G5U5UaV7F"
                     :only-pages? true}))



(defn extract-ref-data-for [title ref-eid ref-block-parents]
  (p "Inside extract ref data for particular page function")
  (let [crumbs-set (into #{} (map #(when (not-empty %)
                                     (replace-block-uids (:string %)))
                               ref-block-parents))
        breadcrumbs (when-not (empty? crumbs-set)
                      (str/join " > " crumbs-set))
        children     (:plain-text (get-children-for {:node ref-eid}))
        full-context (str/join " > " (concat [title
                                              children]
                                       (when-not (nil? breadcrumbs)
                                             [breadcrumbs])))]
    (p "Successfully extracted ref data for page: " title)
    (when (empty? (clojure.set/intersection crumbs-set skip-blocks-with-string))
      {:parents breadcrumbs
       :children children
       :page title
       :full-context full-context})))


(defn get-all-refs-for [{:keys [title block?]}]
   (p "Inside get all refs for particular page function")
   (let [node-eid (if block?
                    (uid->eid title)
                    (get-eid title))
         refs (q '[:find ?title
                   :in $ ?n-eid
                   :where
                   [?d :block/refs ?n-eid]
                   [?d :block/parents ?node]
                   (not [?p :block/children ?node])
                   [?node :node/title ?title]]
                 node-eid)
         res  (atom [])]
     (p "refs" refs)
     (doseq [[index [ref-title]] (map-indexed vector refs)]
       (p "index" index "ref-title" ref-title)
       (let [uid  (title->uid ref-title)]
         (when-let [_ (j/call-in js/window [:roamjs :extension :queryBuilder :isDiscourseNode] uid)]
           (p "----This is discourse node ---" uid)
           (let [page-data (:plain-text (get-children-for {:node ref-title}))
                 pre-s     (str "Referenced discourse node title : " ref-title " \n and its page content: \n ")]
             (swap! res concat [pre-s] page-data [" \n "])))))
     (p "Successfully extracted ref data for all pages ")
     (vec @res)))

(defn get-all-discourse-nodes [])
(defn valid-date?
  "Checks if the given string is a valid date according to the provided format."
  [date-str]
  (let [date (js/Date. date-str)]
    (and (not (js/isNaN (.getTime date)))
      (= date-str (-> date .toISOString (subs 0 (count date-str)))))))



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
  (filter
    #(let [s (second %)]
       (p s)
       (when (clojure.string/starts-with? s (str "[[" t "]]"))
         %))
    dg-nodes))


(comment
  (count (filter
           #(let [uid (str (first %))
                  dg? (j/call-in js/window [:roamjs :extension :queryBuilder :isDiscourseNode] uid)]
               (when dg?
                 %))
           all-title)))


(comment
  (get-all-refs-for {:title "Test: extract query pages"})

  (get-children-for {:node "[[EVD]] - llm test page 2 - [[@source]]"})

  (get-children-for {:node "Test: extract query pages"})

  (get-all-refs-for {:title "G5U5UaV7F"
                     :block? true})
  (get-all-refs-for {:title "KJfDPkNxG"
                     :block? true})
  (get-children-for {:node "[[QUE]] - Testing llm plugin"})

  (get-all-refs-for {:title "[[QUE]] - What is the stoichiometry of Hip1R binding to actin filaments?"})
  (get-all-refs-for {:title "Limit LLM Chat Linked References to dgraph nodes"}))


(defn get-all-data-for [{:keys [title get-linked-refs? block? extract-query-pages? only-pages? vision?]}]
  (p "Inside get all data for particular page function" title)
  (let [children (get-children-for {:node                 title
                                    :block?               block?
                                    :extract-query-pages? extract-query-pages?
                                    :only-pages?          only-pages?
                                    :vision?              vision?})]
      (merge
       (when (not block?)
         {:title title})
       {:body      (if only-pages?
                     (:extracted-query-pages children)
                     (:plain-text children))}
       (when get-linked-refs?
         (when-let [refs (get-all-refs-for {:title  title
                                            :block? (or block? false)})]
           {:linked-refs refs})))))


(defn data-for-nodes [{:keys [nodes get-linked-refs? block? extract-query-pages? only-pages? vision?]}]
  (p "Inside extract data for pages function" nodes get-linked-refs? block? extract-query-pages? only-pages? vision?)
  (let [res (atom [])]
    (doseq [node nodes]
       (swap! res (fn [old-res]
                    (let [args      {:title                node
                                     :get-linked-refs?     (or get-linked-refs? false)
                                     :block?               (or block? false)
                                     :extract-query-pages? (or extract-query-pages? false)
                                     :only-pages?          (or only-pages? false)
                                     :vision?              vision?}
                          _ (p "args" args)
                          node-data (get-all-data-for args)]
                      (cond
                        only-pages? node-data
                        vision?     (conj old-res node-data)
                        :else       (conj old-res (with-out-str
                                                    (print "\n")
                                                    (print node-data)
                                                    (print "\n"))))))))
    @res))



(comment
  ;; ------------------
  (data-for-nodes {:nodes ["Testing"]})
  (data-for-nodes {:nodes ["testing 5"
                           "testing 2"]
                   :vision? true})

  ;; Extract without linked refs or query pages
  (data-for-nodes {:nodes ["Test: extract query pages"]})

  ;; Extract with linked refs
  (data-for-nodes {:nodes ["Test: extract query pages"]
                   :get-linked-refs? true})

  ;; Extract with query pages
  (data-for-nodes {:nodes ["Test: extract query pages"]
                   :extract-query-pages? true})

  ;; Only pages
  (data-for-nodes {:nodes ["Test: extract query pages"]
                   :only-pages? true})

  ;; Extract with linked refs and query pages


  (data-for-nodes {:nodes                ["Test: extract query pages"]
                   :get-linked-refs?     true
                   :extract-query-pages? true})

  ;; Extract with all 3

  (get-all-data-for {:title                "Test: extract query pages"
                     :get-linked-refs?     true
                     :extract-query-pages? true
                     :only-pages?          true})

  (data-for-nodes {:nodes ["UAXMYAZuB"]
                     :get-linked-refs? true
                     :extract-query-pages? true
                     :block? true})

    ;; ------------------


  (data-for-nodes {:title ["Limit LLM Chat Linked References to dgraph nodes"]
                   :get-linked-refs? true})
  (data-for-nodes {:title ["KJfDPkNxG"]
                   :get-linked-refs? true
                   :block? true})
  (data-for-nodes {:title ["1owvT89TK"]
                   :get-linked-refs? true
                   :block? true})
  (data-for-nodes {:title ["llm chat"]
                   :get-linked-refs? true})
  (data-for-nodes {:title ["[[ISS]] - scope other options for the LLM chat interface"]
                   :get-linked-refs? false})
  (data-for-nodes {:title ["[[ISS]] - scope other options for the LLM chat interface"
                           "[[EVD]] - test evd node isDiscourseNode - [[@source]]"]
                   :get-linked-refs? false})

  (data-for-nodes
    {:title ["[[EVD]] - siRNA silenced IRSp53 significantly reduced internalized 10kDa TMR Dextran, while siRNA silenced Swip1 did not in MDA-MB-231 cells. - [[@moreno-layseca2021cargospecific]]",
             "[[CLM]] - Enough number of DNM2 molecules is important for performing endocytosis."]
     :get-linked-refs? false})

  (get-all-refs-for "[[HYP]] - **I am guessing that the ability of arp2/3 complex to bind as frequently as it likes to actin filaments explains the discrepancy between CryoET and simulation measurements**")

  (some? (is-a-page? "[[CLM]] - Enough number of DNM2 molecules is important for performing endocytosis.")))



(defn get-first-pass-context [child get-linked-refs? extract-query-pages? only-pages? vision?]
  (let [cstr             (:string child)
        block-ref?       (when cstr (re-seq (re-pattern "\\(\\(\\(?([^)]*)\\)?\\)\\)") (:string child)))]

    (cond
      (some? (is-a-page? cstr))  (data-for-nodes {:nodes                [(is-a-page? cstr)]
                                                  :get-linked-refs?     get-linked-refs?
                                                  :extract-query-pages? extract-query-pages?
                                                  :only-pages?          only-pages?
                                                  :vision?              vision?})
      (some? block-ref?)         (let [block-uid (second (first block-ref?))]
                                   (data-for-nodes {:nodes                [block-uid]
                                                    :block?               true
                                                    :get-linked-refs?     get-linked-refs?
                                                    :only-pages?          only-pages?
                                                    :extract-query-pages? extract-query-pages?
                                                    :vision?              vision?})))))


(defn extract-for-message-array
  ([messages initial-message]
   (extract-for-message-array messages initial-message ""))
  ([messages initial-message closing-message]
   (let [new-map         (atom [])
         current-message (atom initial-message)]
     (p "messages" messages)
     (doseq [message messages]
       (let [current-image? (extract-markdown-image message)]
         (cond
           (some? (:text current-image?))  (do
                                             (swap! new-map conj {:type "text"
                                                                  :text @current-message})
                                             (reset! current-message "")
                                             (swap! new-map conj {:type      "image_url"
                                                                  :image_url {:url (:url current-image?)}}))
           :else                           (swap! current-message str "\n" message))))
     (p "current message" @current-message)
     (when (not= "" @current-message)
       (swap! current-message str closing-message)
       (swap! new-map conj {:type "text"
                            :text @current-message}))
     @new-map)))


(extract-for-message-array
  ["sid/left-sidebar/personal-shortcuts"
   "![](GMGMMG)"
   "^^Secret message: TESTING 5^^"
   "1"
   "Sections" ""]
  "GM")

(defn generate-roles-for-map [message-map]
  (p "message  map"  message-map)
  (let [body-map (extract-for-message-array
                   (:body message-map)
                   (str "{:title " (:title message-map) "\n :body \n"))
        ref-map   (extract-for-message-array
                    (:linked-refs message-map)
                    (str ":linked-refs \n "))

        res       (vec (concat body-map ref-map))]
   res))


(defn generate-messages-by-role [messages title refs]
  (let [res             (atom [])
        current-message (atom (str "{: title " title " \n  :body \n "))
        ref-map         (extract-for-message-array
                          refs
                          (if (some? refs)
                            (str ":linked-refs \n ")
                            (str ":linked-refs NONE \n "))
                          "}")]
    (doseq [message messages]
      (let [current-image? (when (string? message) (extract-markdown-image message))]
        (p "current image????" current-image? "--" message)
        (cond
          (not (string? message))         (do
                                            (swap! res conj {:type "text"
                                                             :text @current-message})
                                            (reset! current-message "")
                                            (swap! res concat (generate-roles-for-map (first message)))
                                            (swap! res vec))
          (some? (:text current-image?))  (do
                                            (swap! res conj {:type "text"
                                                             :text @current-message})
                                            (reset! current-message "")
                                            (swap! res conj {:type "image_url"
                                                             :image_url {:url (:url current-image?)}}))
          :else                           (swap! current-message str " \n " message))))
    (when (not= "" @current-message)
      (swap! res conj {:type "text"
                       :text @current-message}))
   (vec (concat @res ref-map))))



(defn extract-query-pages [{:keys [context get-linked-refs? extract-query-pages? only-pages? vision?]}]
  (p "extract query pages context:")
  (let [res (atom [])]
    (doseq [child (:children context)]
      (p "child: " child)
      (let [context-with-query-pages (get-first-pass-context
                                       child
                                       get-linked-refs?
                                       extract-query-pages?
                                       only-pages?
                                       vision?)
            _ (p "context with query pages: " context-with-query-pages)
            new-res                  (atom [])]
        (cond
          only-pages? (do
                        (doseq [cstr (:body context-with-query-pages)]
                          (cond
                            (some? (is-a-page? cstr)) (do
                                                        (let [page-data (data-for-nodes
                                                                                 {:nodes                [(is-a-page? cstr)]
                                                                                  :get-linked-refs?     get-linked-refs?
                                                                                  :extract-query-pages? extract-query-pages?
                                                                                  :vision?              vision?})]
                                                          (if vision?
                                                            (swap! new-res conj  page-data)
                                                            (swap! new-res concat page-data))))
                            :else                     (do
                                                        (p "normal string" cstr)
                                                        (swap! new-res conj cstr))))
                        (if vision?
                          (swap! res concat (generate-messages-by-role
                                              @new-res
                                              (:title context-with-query-pages)
                                              (:linked-refs context-with-query-pages)))
                          (swap! res conj (with-out-str
                                            (print "\n")
                                            (print (merge
                                                     (when (some? (:title context-with-query-pages))
                                                       {:title (:title context-with-query-pages)})
                                                     {:body @new-res}
                                                     (when (some? (:linked-refs context-with-query-pages))
                                                        {:linked-refs (:linked-refs context-with-query-pages)})))
                                            (print "\n")))))
          vision?      (do
                         (swap! res concat (generate-messages-by-role
                                             (:body (first context-with-query-pages))
                                             (:title context-with-query-pages)
                                             (:linked-refs context-with-query-pages))))
          :else        (swap! res conj (first context-with-query-pages)))))
    (p "extracted the query pages")
    (if vision?
      @res
      (with-out-str
        (print "\n")
        (print (clojure.string/join " " @res))
        (print "\n")))))




(comment
  (get-child-with-str "KScozVenE" "Context")

  (get-first-pass-context
    {:order 0, :string "[[Test: extract query pages]]", :uid "idyAjL4Xd"},
    true false false true)

  (get-first-pass-context {:string "[[Test: Image to text]]"} true true true true)
  ;; in results graph

  (extract-query-pages
   {:context {:children [{:string "[[testing 5]]"}
                         {:string "[[testing 2]]" }],}
    :get-linked-refs? true
    :extract-query-pages? true
    :only-pages? true})

  (extract-query-pages
   {:context {:children [{:string "[[testing 5]]"}
                         {:string "[[testing 2]]" }],}
    :vision? true})

  (extract-query-pages
   {:context {:children [{:string "[[testing 5]]"}
                         {:string "[[testing 2]]" }],}
    :get-linked-refs? true})

  (extract-query-pages
    {:context {:children [{:string "[[testing 5]]"}
                          {:string "[[testing 2]]" }],}
     :get-linked-refs? true
     :extract-query-pages? true})

  (extract-query-pages
   {:context {:children [{:string "[[testing 5]]"}
                         {:string "[[testing 2]]" }],}
    :get-linked-refs? true
    :extract-query-pages? true
    :only-pages? true
    :vision? false})

  (extract-query-pages
    {:context {:children [{:string "[[testing 5]]"}
                          {:string "[[testing 2]]" }],}
     :get-linked-refs? true
     :extract-query-pages? true
     :only-pages? false
     :vision? true})

  (extract-query-pages
    {:context {:children [{:order 0, :string "((G5U5UaV7F))", :uid "idyAjL4Xd"}],}
     :get-linked-refs? true
     :extract-query-pages? true
     :only-pages? true})

  (extract-query-pages
    {:context              {:children [{:order 0, :string "[[Test: extract query pages]]", :uid "idyAjL4Xd"}],}
     :get-linked-refs?     true
     :extract-query-pages? true
     :only-pages?          true
     :vision?              true})
  (extract-query-pages
    {:context              {:children [{:order 0, :string "[[Test: extract query pages]]", :uid "idyAjL4Xd"}],}
     :get-linked-refs?     true
     :extract-query-pages? true
     :only-pages?          true
     :vision?              false})
  (extract-query-pages
    {:context              {:children [{:order 0, :string "[[Test: extract query pages]]", :uid "idyAjL4Xd"}],}
     :get-linked-refs?     true
     :extract-query-pages? true
     :only-pages?          true
     :vision?              false})
  (extract-query-pages
    {:context              {:children [{:order 0, :string "[[Testing]]", :uid "idyAjL4Xd"}],}
     ;:get-linked-refs?     true
     ;:extract-query-pages? true
     :only-pages?          true})
     ;:vision?              false})

  (extract-query-pages
    {:context              {:children [{:order 0, :string
                                        "[[[[EVD]] - NWASP was found around clusters of clathrin heavy chain by TIRF microscopy + super resolution microscopy - [[@leyton-puig2017flat]]]]"
                                        , :uid "gYvoYlSJi"}],}
     :get-linked-refs?     true
     :extract-query-pages? true
     :only-pages?          false
     :vision?              false})

  (extract-query-pages
    {:context              {:children [{:order 0, :string "[[Test: Image to text]]", :uid "idyAjL4Xd"}],}
     :get-linked-refs?     true
     :extract-query-pages? true
     :only-pages?          false
     :vision?              true})

 (extract-query-pages
   {:context              {:children [{:order 0, :string "((Qr3-5Zogr))" #_"((4y3qbZBo1))", :uid "idyAjL4Xd"}],}
    :vision? true
    :only-pages? false}))




