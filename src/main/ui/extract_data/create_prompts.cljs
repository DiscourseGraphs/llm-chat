(ns ui.extract-data.create-prompts
  (:require [ui.utils :refer [p gen-new-uid extract-data create-struct]]))

;; prompt exists
;; find the prompt guide
;;
;;


(defn get-dg-node-types []
  {:summary ""
   :with-credentials ""})
(defn extract-example []
  "")
(defn extract-lab-ontology []
  "")
(defn dg-nodes-format []
  "")


(defn manual-prompt-guide [action-button-uid]
  (let [entry-point "Our lab uses Roam Research to organize our collaboration and knowledge sharing."
        dg-nodes    (str "We capture "
                      (:summary get-dg-node-types)
                      "\n on separate pages in Roam. Each page has a title summarizing the key insight. We call these discourse nodes.
                      Discourse graphs are an information model for bodies of knowledge that emphasize discourse moves (such as questions, claims, and evidence), and relations (such as support or opposition), rather than papers or sources as the main units.")
        example     (str "<example> \n "
                      (extract-example)
                      "\n </example>")
        your-job    (str "<your-job> \n
                       Based on the text and images provided, propose some new discourse nodes.
                      </your-job>")
        dg-nodes-description (str "<types-of-discourse-nodes>"
                              (:with-definitions get-dg-node-types)
                              "</types-of-discourse-nodes>")
        lab-ontology (str "<instructions> \n
                       <lab-ontology> \n"
                       (extract-lab-ontology)
                      "\n </lab-ontology>")
        response-format (str "<expected-response-format> \n
                              - follow the following format, this is format of the following lines `node type - format to follow if the node is of this type`. For each suggestion put it on a new line."
                              (dg-nodes-format)
                              " <Important-note> replace the `Source` with actual source. </important-note>\n</expected-response-format>")
        general-important-instructions (str "<general-important-instructions>\n1. following the format does not mean degrading your answer quality. We want both follow the format and high quality suggestions. Make sure your {content} draws directly from the text and images provided.\n2. Please only reply with discourse node suggestions, not explanations, keep them high quality. \n</general-important-instructions>\n</instructions>\n"
                                         "\n Extracted data from pages:
                                         <data-from-pages> \n")
        combined-prompt (str
                          entry-point
                          dg-nodes
                          example
                          your-job
                          dg-nodes-description
                          lab-ontology
                          response-format
                          general-important-instructions)

        prompt-uid (gen-new-uid)
        struct {:s "Prompt"
                :c [{:s "Pre-prompt"
                     :c [{:s combined-prompt
                          :u prompt-uid}]}]}]

    (create-struct
        struct
        action-button-uid
        nil
        false
        #(p "created new prompt for dg this page"))
    combined-prompt))



