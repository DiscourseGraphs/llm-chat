(ns ui.extract-data.create-prompts
  (:require [ui.utils :refer [p q update-block-string model-mappings call-llm-api gen-new-uid extract-data create-struct]]
            [cljs.core.async :refer [chan go <! >! put! close!]]
            [applied-science.js-interop :as j]))


(defn ask-llm-for-context [prompt]
  (let [messages [{:role    "user"
                   :content prompt}]
        settings {:model       (get model-mappings "gpt-4-vision")
                  :temperature 0.9
                  :max-tokens  400}
        out-ch  (chan)]
    (p "***asking llm for context***" prompt)
    (call-llm-api
      {:messages messages
       :settings settings
       :callback (fn [response]
                   (go
                     (let [body-text (-> response :body)]
                       (p "***llm response***" body-text)
                       (put! out-ch body-text)
                       (close! out-ch))))})
    out-ch))

(comment 
  (q '[:find (pull ?e [*])
       :where [?e :block/uid "DoIcrR_MO"]]))


(defn extract-node-info
  [nodes]
  (->> nodes
    (map (fn [node]
           {:description (:description node "")
            :text (:text node "")
            :format (:format node "")}))
    (filter (fn [node]
             (or (not-empty (:description node))
                 (not-empty (:text node))
                 (not-empty (:format node)))))))

(def general-dg-system-prompt "Given the following information about different type of nodes,
                                 their description (if present) and the format.")
(def dg-nodes-example
 " for <example> if the <node-info> is (excluding the codeblock marking)
 ```
   {:description \"is a research observation, include source of your result\", :text \"Result\", :format \"[[RES]] - {content}\"}
   {:description \" is a testable claim \", :text \"Hypothesis\", :format \"[[HYP]] - {content}\"}\n
   {:description \"is a desired experiment, or \"request for experiment\".\", :text \"Issues\", :format \"[[ISS]] - {content}\"}\n
   ```
   </example>")


(defn get-dg-node-types [nodes-info loading-message-uid]
  (go

   (let [summary-prompt          (str
                                   general-dg-system-prompt
                                  "I want you to create summary of all the
                                  nodes provided. The output should be of the <format> `artifact (Short-form-used-in-format)`
                                  </format>"
                                  dg-nodes-example
                                  "then you output
                                 `result (RES), hypotheses (HYP), and conclusions (CON)` </example>
                                 <important-note> I only want the result in requested format please DO NOT include any other
                                  information in the output. </important-note>
                                 following is the actual data
                                 <node-info> /n ")
         combined-prompt         (str summary-prompt
                                      (str nodes-info)
                                      "</node-info")
         llm-generated-summary   (<! (ask-llm-for-context combined-prompt))]

     ;; do:  With definitions Following will not work for now because we don't have a way to eraisly extract the examples of particular type.

     (p "discourse nodes" nodes-info)
     (p "summary" llm-generated-summary)
     (update-block-string
       loading-message-uid
       (str "LLM extracted summary of the Discourse graph node types are: "
            llm-generated-summary))
     {:summary  llm-generated-summary
      :with-definitions ""})))


(defn extract-example []
  (let [node-regex    (re-pattern (str "^\\[\\[QUE\\]\\] - (.*?)$"))
        some-question (:title (ffirst (q '[:find (pull ?e [:node/title :block/uid])
                                           :in $ ?node-regex
                                           :where
                                             [?e :node/title ?tit]
                                             [(re-find ?node-regex ?tit)]]
                                         node-regex)))]
    (str "a QUE page may ask \""
         some-question
         "\" This could link to a HYP page proposing a molecular binding mechanism as a hypothesis. The HYP page would in turn link to RES pages that either support or oppose the hypothesis.")))


(defn extract-lab-ontology [nodes-info loading-message-uid]
  (go
    (let [ontology-prompt (str
                            general-dg-system-prompt
                            "I want you to extract the lab ontology in the
                           <format>
                           (Chronological node number) (Short-form-used-in-format) Description-extracted-from-the-data
                           - sub-description if provided
                           (Next Chronological node number) (Short-form-used-in-format) Description-extracted-from-the-data
                           - sub-description if provided
                           </format>
                           "
                            dg-nodes-example
                            "Then your output should be (excluding the codeblock):
                            ```
                            <exactly-follow-our-lab-ontology>
                            1. Result (RES) - is a research observation
                                - should include source of your result
                            2. Hypothesis (HYP) - is a testable claim \n
                            3. Issue (ISS) - is a desired experiment,or request for experiment\n
                            </exactly-follow-our-lab-ontology>
                            ```
                            Following is the actual data
                            <node-info>
                         ")
          combined-prompt (str ontology-prompt
                               (str nodes-info)
                               "</node-info>")
          llm-generated-prompt (<! (ask-llm-for-context combined-prompt))]
      (p "lab ontology" llm-generated-prompt)
      (update-block-string
        loading-message-uid
        (str "LLM extracted the lab ontology as: "
             llm-generated-prompt))
      llm-generated-prompt)))

(defn dg-nodes-format [nodes-info]
  (reduce
    (fn [c n]
      (str c "\n" (:format n)))
    ""
    nodes-info))

(comment
  (let [all-nodes (-> (j/call-in js/window [:roamjs :extension :queryBuilder :getDiscourseNodes])
                    (js->clj :keywordize-keys true))]
    (extract-node-info all-nodes)
    (dg-nodes-format all-nodes)))

(defn manual-prompt-guide [action-button-uid loading-message-uid]
  (go
   (let [get-all-discourse-nodes (-> (j/call-in js/window [:roamjs :extension :queryBuilder :getDiscourseNodes])
                                   (js->clj :keywordize-keys true))
         nodes-info              (extract-node-info get-all-discourse-nodes)

         _ (update-block-string
                          loading-message-uid
                          "Querying for the discourse graph node types defined in the graph, then asking the llm to present it in predefined summary format.")
         dg-node-types           (<! (get-dg-node-types nodes-info loading-message-uid))
         summary                 (:summary dg-node-types)
         entry-point             "Our lab uses Roam Research to organize our collaboration and knowledge sharing."
         dg-nodes                (str "We capture "
                                   summary
                                   "\n on separate pages in Roam. Each page has a title summarizing the key insight. We call these discourse nodes.
                                        \n Discourse graphs are an information model for bodies of knowledge that emphasize discourse moves (such as questions, claims, and evidence), and relations (such as support or opposition), rather than papers or sources as the main units.")
         example                 (str "\n <example> \n "
                                   (extract-example)
                                   "\n </example> \n")
         _                      (update-block-string
                                   loading-message-uid
                                   "Extracted the example of a question from the graph.")
         your-job                (str "<your-job>
                                      \n Based on the text and images provided, propose some NEW discourse nodes. <DO NOT> suggest existing discourse nodes from the context </ DO NOT> but to propose NEW creative discourse nodes, make connections that are present but not formalised yet.
                                      \n </your-job> \n")
         _                      (update-block-string
                                  loading-message-uid
                                  "Using the discourse graph node types defined in the graph, then asking the llm to define the ontology used in our lab.")
         lab-ontology           (str "\n <instructions> \n
                                         \n <lab-ontology> \n"
                                     (<! (extract-lab-ontology nodes-info loading-message-uid))
                                     "\n </lab-ontology> \n")
         response-format        (str "<expected-response-format> \n
                                          \n - follow the following format, this is format of the following lines `node type - format to follow if the node is of this type`. For each suggestion put it on a new line."
                                  (dg-nodes-format nodes-info)
                                  " \n <Important-note> replace the `Source` with actual source. </important-note>\n \n</expected-response-format>")
         general-instructions  (str "\n <general-important-instructions> \n 1. following the format does not mean degrading your answer quality. We want both follow the format and high quality suggestions. Make sure your {content} draws directly from the text and images provided.\n2. Please only reply with discourse node suggestions, not explanations, keep them high quality. \n</general-important-instructions>\n</instructions>\n"
                                    "\n Extracted data from pages:
                                         \n <data-from-pages> \n")
         combined-prompt       (str
                                 entry-point
                                 dg-nodes
                                 example
                                 your-job

                                 ;; can't use as of now
                                 ;dg-nodes-description
                                 lab-ontology
                                 response-format
                                 general-instructions)]
     (update-block-string
       loading-message-uid
       "LLM created a prompt for this action in your graph, you can see it in your left-sidebar, please check and modify the prompt for future use.")
     (let [prompt-uid (gen-new-uid)
           pprompt-uid (gen-new-uid)
           struct {:s "Prompt"
                   :c [{:s "Pre-prompt"
                        :u prompt-uid
                        :c [{:s combined-prompt
                             :u pprompt-uid}]}]}]
       (create-struct
         struct
         action-button-uid
         prompt-uid
         true
         #(p "created new prompt for dg this page")
         1)
       combined-prompt))))
