(ns ui.components.search-pinecone
  (:require [reagent.core :as r]
            [cljs-http.client :as http]
            [cljs.core.async :refer [take!]]
            [applied-science.js-interop :as j]
            [ui.utils :refer [p]]
            ["@blueprintjs/core" :as bp :refer [InputGroup MenuItem MenuDivider Button Popover Menu]]))

(defn search-pinecone []
  (let [query    (r/atom "")
        loading? (r/atom false)
        results  (r/atom [])]
    (fn []
        [:> Popover 
         {:isOpen (not (empty? @results))
          :position "bottom"
          :onClose #(reset! results [])}

         [:> InputGroup 
          {:placeholder "Search..."
           :leftIcon "search"
           :onChange #(reset! query (.. % -target -value))
           :value @query
           :type "search"
           :fill true
           :disabled @loading?
           :rightElement (r/as-element
                           [:> Button {:icon "arrow-right"
                                       :onClick (fn []
                                                    (when-not @loading?
                                                      (reset! loading? true)
                                                      (let [url      "https://roam-llm-chat-falling-haze-86.fly.dev/get-openai-embeddings"
                                                            headers  {"Content-Type" "application/json"}
                                                            res-ch   (http/post url {:with-credentials? false
                                                                                     :headers headers
                                                                                     :json-params {:input [@query]
                                                                                                   :top-k 3}})]
                                                        (take! res-ch (fn [res]
                                                                        (reset! loading? false)
                                                                        (let [embeddings (first (js->clj (:body res) :keywordize-keys true))]
                                                                          (p "EMBEDDINGS RES: " embeddings)
                                                                          (reset! results embeddings)))))))
                                       :disabled @loading?
                                       :loading @loading?
                                       :minimal true
                                       :style {:flex "1 1 1"
                                               :padding "4px"}}])}]
         [:> Menu
                    {:style {:width "80em"}}
                    (for [[idx result] (map-indexed vector @results)]
                       ^{:key (str (:id result))}
                       [:<>
                        [:> MenuItem {:text (str (-> result :metadata :title))
                                      :style {:padding "5px"}
                                      :on-click #(do
                                                   (p "clicked menu item" result "{{" (str (-> result :metadata :title)))
                                                   (-> (j/call-in js/window [:roamAlphaAPI :ui :mainWindow :openPage]
                                                                  (clj->js {:page {:title (str (-> result :metadata :title))}}))
                                                       (.then (fn [] (p "opened up the page"))))
                                                   (reset! results []))}]
                        [:> MenuDivider]])]])))
         
      
                                   ;; You can add more actions here
                                     





