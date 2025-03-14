(ns ui.extract-data.graph-overview-ai
  (:require
    [applied-science.js-interop :as j]
    [ui.utils :as utils :refer [q]]))

(defn is-discourse-node [s]
  ;(p "-->" s)
  (let [node ["QUE" "CLM" "EVD" "RES" "ISS" "HYP" "EXP" "CON"]
        node-regex (re-pattern (str "\\[\\[" (clojure.string/join "\\]\\]|\\[\\[" node) "\\]\\]"))
        src-regex (re-pattern "^@[^\\s]+")]
    (if (or (re-find node-regex s)
          (re-find src-regex s))
      s
      nil)))

(defn discourse-nodess [s]
  (let [node ["QUE" "CLM" "EVD" "RES" "ISS" "HYP" "EXP" "CON"]
        node-regex (re-pattern (str "\\[\\[(" (clojure.string/join "|" node) ")\\]\\]"))
        src-regex (re-pattern "@[^\\s]+")] ; Removed the '^' to match '@' anywhere in the string
    (if (or (re-find node-regex s)
          (re-find src-regex s))
      s
      nil)))


(comment
  (is-discourse-node "a [QUE]] - Is this a question?")
  (is-discourse-node "[[QUE]] - How do Hip1R binding kin)etics and periodicity affect endocytic actin architecture and integrity?")
  (is-discourse-node "@Simulations varying "))

(defn generate-in-query-pattern [depth]
  (let [queries (concat
                  [[(symbol  "?e"  ) :node/title (symbol (str "?page"))]]
                  (mapcat (fn [d]
                            (let [base-index (- d 1)]
                              [[(symbol (str "?d" base-index)) :block/refs (symbol
                                                                             (if (= 0 base-index)
                                                                               "?e"
                                                                               (str "?r"  (- base-index 1))))]
                               [(symbol (str "?d" base-index)) :block/page (symbol (str "?r" base-index))]]))
                    (range 1 (inc depth)))
                  [[(symbol (str "?r" (- depth 1) )) :node/title (symbol (str "?title"))]])]
    (pr-str (vec (concat [:find (symbol (str "?title" )) :in '$ '?page :where]
                   queries)))))

(comment (generate-in-query-pattern 2))

(defn get-in-refs [page-name depth]
  (let [qry-res  (q
                   (generate-in-query-pattern depth)
                   page-name)
        filtered-discourse-nodes (reduce (fn [acc x]
                                           (if (some? (is-discourse-node (first x)))
                                             (conj acc (first x))
                                             acc))
                                   #{}
                                   qry-res)]

   ; (p "In qry res count:" (count qry-res))
    filtered-discourse-nodes))

(comment
  (get-in-refs
    "[[QUE]] - How does frequency of Arp2/3 complex binding to actin filaments affect endocytic actin architecture and integrity?"
    0))

(defn generate-out-query-pattern [depth]
  (let [queries (concat
                  [[(symbol  "?p"  ) :node/title (symbol (str "?page"))]]
                  (mapcat (fn [d]
                            (let [base-index (- d 1)]
                              [[(symbol (str "?d" base-index)) :block/refs (symbol (str "?e"  base-index))]
                               [(symbol (str "?d" base-index)) :block/page (symbol
                                                                             (if (= 0 base-index)
                                                                               "?p"
                                                                               (str "?e" (- base-index 1))))]]))
                    (range 1 (inc depth)))
                  [[(symbol (str "?e" (- depth 1) )) :node/title (symbol (str "?title"))]])]
    (pr-str (vec (concat [:find (symbol (str "?title" )) :in '$ '?page :where]
                   queries)))))



(comment
  (generate-out-query-pattern 3)

  (q
    (generate-out-query-pattern 1)
    "[[QUE]] - How does frequency of Arp2/3 complex binding to actin filaments affect endocytic actin architecture and integrity?"))

(defn get-out-refs [page-name depth]
  (let [qry-res  (q
                   (generate-out-query-pattern depth)
                   page-name)
        filtered-discourse-nodes (reduce (fn [acc x]
                                           (if (some? (is-discourse-node (first x)))
                                             (conj acc (first x))
                                             acc))
                                   #{}
                                   qry-res)]
    ;(p "Out qry res count:" (count qry-res))
    filtered-discourse-nodes))


(comment
  (get-out-refs
    "[[QUE]] - How does frequency of Arp2/3 complex binding to actin filaments affect endocytic actin architecture and integrity?"
    1))


(defn get-explorer-pages []
  ;(p "get explorer pages")
  (let [page-name (str (js->clj (first (j/call-in js/window [:roamAlphaAPI :ui :graphView :wholeGraph :getExplorePages]))))
        [in out]  (->> (j/call js/document :querySelectorAll ".bp3-slider-handle > .bp3-slider-label")
                    (map (fn [x]
                           (j/get x :textContent))))
        in-pages (if (> in 0)
                   (get-in-refs page-name  (js/parseInt in))
                   #{})
        out-pages (if (> out 0)
                    (get-out-refs page-name (js/parseInt out))
                    #{})]
    ;(p "in out - pages" in out)
    {:page-name page-name
     :in-pages in-pages
     :out-pages out-pages}))


(comment
  (get-explorer-pages))