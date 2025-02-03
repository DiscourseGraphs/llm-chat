(ns ui.components.cytoscape
 (:require [reagent.core :as r]
           [reagent.dom :as rd]
           [ui.utils :refer [q]]
           ["@blueprintjs/core" :as bp :refer [ControlGroup Checkbox Tooltip HTMLSelect Button ButtonGroup Card Slider Divider Menu MenuItem Popover MenuDivider]]
           [ui.utils :refer [p get-title-with-uid]]
           [ui.extract-data.dg :refer [determine-node-type all-dg-nodes get-all-discourse-node-from-akamatsu-graph-for]]
   ["cytoscape" :as cytoscape]
   ["cytoscape-cose-bilkent" :as cose-bilkent]))

(.use cytoscape cose-bilkent)
(def edge-colors
  {"InformedBy" "#1f78b4"
   "Informs" "#33a02c"
   "OpposedBy" "#e31a1c"
   "SupportedBy" "#ff7f00"
   "Reproduces" "#6a3d9a"
   "Supports" "#b15928"
   "Opposes" "#a6cee3"})

(def node-colors
  {:QUE "#4e79a7"
   :CLM "#f28e2b"
   :EVD "#e15759"
   :HYP "#76b7b2"
   :ISS "#59a14f"
   :CON "#edc948"
   :RES "#b07aa1"})

(defn convert-to-cytoscape-nodes [unique-nodes all-nodes]
  (reduce (fn [acc [x {:keys [uid title type]}]]
            (assoc acc title {:data
                              {:id uid
                               :label title
                               :bwidth "0px"
                               :bstyle "solid"
                               :bcolor "black"
                               :color (node-colors type)}}))
    all-nodes
    unique-nodes))

(defn convert-to-cytoscape-edges [data]
  (mapv (fn [[source label target]]
          {:data {:id (str (:uid source) "-" (:uid target))
                  :source (:uid source)
                  :target (:uid target)
                  :label label
                  :color (edge-colors label)}
           :group "edges"})
    data))

(defn get-node-data [node-title]
  (let [res       (first (get-title-with-uid node-title))
        node-data (first (map (fn [{:keys [uid title]}]
                                {:data {:id uid
                                        :label title
                                        :color (node-colors (determine-node-type title))
                                        :bwidth "3px"
                                        :bstyle "solid"
                                        :bcolor "black"}})
                           res))]
    (when (some? node-data)
      {node-title node-data})))

(defn random-uid [length]
  (let [chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        chars-len (count chars)]
    (apply str (repeatedly length #(nth chars (rand-int chars-len))))))

(defn suggested-nodes [nodes]
  (let [res (atom {})]
    (doseq [node nodes]
      (let [title (:string node)
            uid (:uid node)
            node-data {title {:data {:id uid
                                     :label title
                                     :color "white"
                                     :bwidth "2px"
                                     :bstyle "dashed"
                                     :bcolor "black"}}}]
       (reset! res (merge node-data @res))))
    @res))





(defn get-cyto-format-data-for-node [{:keys [nodes extra-data]}]
  (let [all-nodes (atom {})
        all-edges (atom [])
        extra-data (if (some? extra-data) extra-data [])]
    (doall
     (map
       (fn [node]
           (p "Get cyto format processing: " node)
           (let [res (get-all-discourse-node-from-akamatsu-graph-for node)
                 edges (convert-to-cytoscape-edges (:edges res))
                 nodes (convert-to-cytoscape-nodes (:nodes res) @all-nodes)
                 node-data (get-node-data node)]

             (do
              (reset! all-nodes (merge node-data @all-nodes))
              (when (not (empty? nodes))
                (reset! all-nodes (merge nodes @all-nodes)))
              (swap! all-edges concat edges))))
       nodes))
    (into [] (concat
               (vals @all-nodes)
               @all-edges
               extra-data))))


(comment
 (:nodes (get-all-discourse-node-from-akamatsu-graph-for "[[RES]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I "))

 (get-cyto-format-data-for-node {:nodes ["[[HYP]] - Myosin-I directly applies force on the actin network via its power stroke to assist in endocytosis"]})
 (get-cyto-format-data-for-node {:nodes ["[[RES]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I "]}))


#_(def example-elements
    (get-cyto-format-data-for-node
     {:nodes ["[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding  - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]"
              "[[RES]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I"
              "[[RES]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I "
              "[[HYP]] - Myosin-I directly assists with endocytosis by using its power stroke to apply a force directly assisting internalization"
              "[[HYP]] - Myosin-I directly applies force on the actin network via its power stroke to assist in endocytosis"
              "[[HYP]] - Transient actin-binding alone is sufficient for myosin to assist endocytosis"
              ;; Test
              #_"[[CLM]] - Actin polymerization participates in the scission of CLIC endocytic tubules"
              #_"[[QUE]] - Does actin polymerization adjacent to CLICs help to form CLIC tubules or scission of CLICs?"
              #_"[[EVD]] - hPSCs were found to exert strong inward-directed mechanical forces on the ECM at specific locations coinciding with ventral stress fibers at the colony edge.  - [[@narva2017strong]]"]}))


(def default-layout
  {:name "cose-bilkent"
   :nodeDimensionsIncludeLabels true
   :padding 10
   :gravityRange 1.6
   :idealEdgeLength 100
   :fit true})

(def llm-suggestions0
  {
   "[[RES]] - Increasing membrane tension from 2 pN/nm to 2000 pN/nm in simulations showed a broader assistance by myosin in internalization - [@SlidePresentation]"
   [
    ["[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" "Supports"]
    ["[[CON]] - Higher membrane tension facilitates the nucleation of more actin filaments" "Supports"]
    ["[[CON]] - Higher membrane tension facilitates the nucleation of more actin filaments" "Supports"]]

   "[[RES]] - Resistance to internalization increased as myosin unbinding rate decreased at higher membrane tension in simulations - [@SlidePresentation]"
   [
    ["[[RES]] - Increasing the value of force-attenuated capping in endocytic actin simulations at elevated membrane tension showed reduced variability in the cumulative force on growing filaments - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" "Supports"]
    ["[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" "Supports"]
    ["[[RES]] - Implementing force-attenuated capping in endocytosis simulations led to a decrease in mean internalization efficiency at tension 1000 pN/µm - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" "Opposes"]]

   "[[RES]] - At 20 pN/nm membrane tension, areas with low myosin unbinding rates had decreased internalization resistance - [@SlidePresentation]"
   [
    ["[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" "Supports"]
    ["[[RES]] - The addition of type-I myosin to endocytosis simulations increased endocytic internalization in regimes of fast myosin unbinding rate and low catch bond sensitivity - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" "Supports"]
    ["[[RES]] - Addition of Myosin-I does not systematically relieve stalling of actin filament elongation" "Opposes"]]

   "[[ISS]] - Investigate the relationship between myosin catch bonding parameters and internalization efficiency in live cell experiments"
   [
    ["[[HYP]] - Type-I myosin modulates the nucleation rate of endocytic actin filaments" "InformedBy"]
    ["[[HYP]] - Myosin-I helps to modulate the rate of nucleation of new branched actin filaments" "InformedBy"]
    ["[[HYP]] - Myosin I helps lift actin away from the plasma membrane to “unstall” it and increase its polymerization rate" "InformedBy"]]

   "[[HYP]] - Myosin assists more broadly in membrane internalization under higher tension conditions"
   [
    ["[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" "SupportedBy"]
    ["[[RES]] - Implementing force-attenuated capping in endocytosis simulations led to a decrease in mean internalization efficiency at tension 1000 pN/µm - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" "InformedBy"]
    ["[[EVD]] - Increased membrane tension due to hypotonic swelling resulted in increased clathrin coat lifetimes and reduction in standard deviation of clathrin growth rates. - [[@ferguson2017mechanoregulation]]" "InformedBy"]]})

(def llm-suggestions
  {
   "[[RES]] - Increasing membrane tension from 2 pN/nm to 2000 pN/nm in simulations showed a broader assistance by myosin in internalization - [@SlidePresentation]"
   [
    ["[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]", "Supports"],
    ["[RES]] - The addition of type-I myosin to endocytosis simulations increased endocytic internalization in regimes of fast myosin unbinding rate and low catch bond sensitivity - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]", "Supports"],
    ["[RES]] - Simulated actin filament capping rate reduced exponentially as a function of force when force-attenuated capping was implemented - [[@cytosim/vary force-dependent capping in endocytosis simulation]]", "Supports"]]

   "[[RES]] - Resistance to internalization increased as myosin unbinding rate decreased at higher membrane tension in simulations - [@SlidePresentation]"
   [
    ["[RES]] - Addition of Myosin-I does not systematically relieve stalling of actin filament elongation", "Supports"],
    ["[RES]] - With high catch bonding and low unbinding rates, myosin-I contributes to increased stalling of endocytic actin filaments, greater nucleation rates, and less pit internalization", "Supports"],
    ["[RES]] - Implementing force-attenuated capping in endocytosis simulations led to an increase in the cumulative number of uncapped filaments at tension 150 pN/µm - [[@cytosim/vary force-dependent capping in endocytosis simulation]]", "Supports"]]

   "[[RES]] - At 20 pN/nm membrane tension, areas with low myosin unbinding rates had decreased internalization resistance - [@SlidePresentation]"
   [
    ["[RES]] - Under high catch bonding regimes, simulated Type-I myosin caused actin filaments to orient less perpendicular to the base of the endocytic pit - [[@analysis/report endocytic actin network architecture ± myosin-I]]", "Supports"],
    ["[RES]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I", "Supports"],
    ["[RES]] - The addition of type-I myosin to endocytosis simulations increased endocytic internalization in regimes of fast myosin unbinding rate and low catch bond sensitivity - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]", "Supports"]]

   "[[ISS]] - Investigate the relationship between myosin catch bonding parameters and internalization efficiency in live cell experiments"
   [
    ["[HYP]] - Myosin I’s rapid ATPase cycle is crucial to allow actin to __reorganize__ during endocytosis to accommodate changing loads in a stochastic environment.", "Informs"],
    ["[HYP]] - Myosin-I directly applies force on the actin network via its power stroke to assist in endocytosis", "Informs"],
    ["[HYP]] - Myosin I helps lift actin away from the plasma membrane to “unstall” it and increase its polymerization rate", "Informs"]]

   "[[HYP]] - Myosin assists more broadly in membrane internalization under higher tension conditions"
   [
    ["[EVD]] - Increased membrane tension due to hypotonic swelling resulted in increased clathrin coat lifetimes and reduction in standard deviation of clathrin growth rates. - [[@ferguson2017mechanoregulation]]", "Supports"],
    ["[EVD]] - Adding force-dependent capping with capping force value of 0.0063 pN did not appreciably change the number of growing filaments in endocytosis simulations - [[@cytosim/vary force-dependent capping in endocytosis simulation]]", "Informs"],
    ["[EVD]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I", "Supports"]]

   "[[CLM]] - High membrane tension facilitates myosin’s role in overcoming resistance to internalization"
   [
    ["[EVD]] - Increased membrane tension due to hypotonic swelling resulted in increased clathrin coat lifetimes and reduction in standard deviation of clathrin growth rates. - [[@ferguson2017mechanoregulation]]", "Supports"],
    ["[EVD]] - Increased membrane tension results in slower dynamics of clathrin coated structures. - [[@kaplan2022load]]", "Opposes"],
    ["[EVD]] - Simulated endocytic internalization efficiency increased from 0.1% to 1.5% as a function of increasing membrane tension - [[@akamatsu2020principles]]", "Supports"]]})


(def llm-suggestions-2
  {"[[RES]] - Increasing membrane tension from 2 pN/nm to 2000 pN/nm in simulations showed a broader assistance by myosin in internalization - [@SlidePresentation]"
   [{:uid "HvCY93DFk" :title "[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding  - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" :label "Supports"}
    {:uid "lUCSGlEMg" :title "[[RES]] - Implementing FAC = 8.828 pN-1 in endocytosis simulations significantly increased the average internalization velocity between 0-5s at elevated membrane tension - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "Supports"}
    {:uid "l8wVCJv3H" :title "[[RES]] - Increasing membrane tension from 150 to 1000 pN/µm lead to an increase in mean internalization efficiency in endocytosis simulations, both without and with FAC implemented - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "Supports"}]
   "[[RES]] - Resistance to internalization increased as myosin unbinding rate decreased at higher membrane tension in simulations - [@SlidePresentation]"
   [{:uid "aHxTg7kjH" :title "[[RES]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I." :label "Supports"}
    {:uid "JsrQTBxFb" :title "[[RES]] - Implementing force attenuated capping increased the number of filaments experiencing antagonistic force in cytosim simulations. - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "Supports"}
    {:uid "JrQ9Kcu46" :title "[[RES]] - with a binding rate of 2.7 events per second, binding radius of 50 nm, and cutting rate of 0.09 events per second, 10 actin filaments of length 10 microns were severed at a rate of 0.004 and 0.007 events per second, for 150 and 250 nM cofilin respectively." :label "Supports"}]

   "[[RES]] - At 20 pN/nm membrane tension, areas with low myosin unbinding rates had decreased internalization resistance - [@SlidePresentation]"
   [{:uid "fA6UTS31j" :title "[[RES]] - Implementing force-attenuated capping in endocytosis simulations did not change the antagonistic force/filament experiencing force at tension 150 pN/µm - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "Supports"}
    {:uid "D-uoSWpAD" :title "[[RES]] - For baseline levels of membrane tension, there was an inverse relationship between the change in number of filaments and change in internalization for different unbinding and catch bonding properties of myosin-I." :label "Supports"}
    {:uid "g7DRfmHot" :title "[[RES]] - Implementing FAC = 8.828 pN-1 in endocytosis simulations drastically reduced the cumulative antagonistic force on capped filaments in the hip1r-associated network - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "Supports"}]

   "[[ISS]] - Investigate the relationship between myosin catch bonding parameters and internalization efficiency in live cell experiments"
   [{:uid "Oq631xMxn" :title "[[CON]] - Type-I myosin modulates the nucleation rate of endocytic actin filaments" :label "InformedBy"}
    {:uid "K2o4pAebP" :title "[[HYP]] - Myosin-I helps to modulate the rate of nucleation of new branched actin filaments" :label "InformedBy"}
    {:uid "as9B0xYLG" :title "[[HYP]] - Increasing membrane tension increases the load on the ends of growing actin filaments" :label "InformedBy"}]

   "[[HYP]] - Myosin assists more broadly in membrane internalization under higher tension conditions"
   [{:uid "HvCY93DFk" :title "[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding  - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" :label "SupportedBy"}
    {:uid "l8wVCJv3H" :title "[[RES]] - Increasing membrane tension from 150 to 1000 pN/µm lead to an increase in mean internalization efficiency in endocytosis simulations, both without and with FAC implemented - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "SupportedBy"}
    {:uid "lUCSGlEMg" :title "[[RES]] - Implementing FAC = 8.828 pN-1 in endocytosis simulations significantly increased the average internalization velocity between 0-5s at elevated membrane tension - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "SupportedBy"}]

   "[[CLM]] - High membrane tension facilitates myosin’s role in overcoming resistance to internalization"
   [{:uid "HvCY93DFk" :title "[[RES]] - Under increasing values of membrane tension, endocytic myosin-I assisted endocytosis under a wider range of values of unbinding rate and catch bonding  - [[@cytosim/simulate myosin-I with calibrated stiffness value and report unbinding force vs. unbinding rate]]" :label "SupportedBy"}
    {:uid "l8wVCJv3H" :title "[[RES]] - Increasing membrane tension from 150 to 1000 pN/µm lead to an increase in mean internalization efficiency in endocytosis simulations, both without and with FAC implemented - [[@cytosim/vary force-dependent capping in endocytosis simulation]]" :label "SupportedBy"}
    {:uid "Nrk8MP8l7" :title "[[CON]] - Force-attenuated capping = 8.828 pN-1 significantly increases average internalization velocity for the first 5s of endocytosis simulations, at elevated, but not baseline, membrane tension." :label "SupportedBy"}]})



(defn unique-maps [maps]
  (vals (into {} (map (fn [m] [(m :uid) m]) maps))))

(comment
  (count (unique-maps (flatten (vals llm-suggestions-2)))))



(defn suggested-edges [data]
  (mapcat (fn [[source targets]]
            (map (fn [{:keys [uid title label]}]
                   {:data
                    {:id     uid
                     :source source
                     :target  title
                     :relation label
                     :color    (edge-colors label)}})
              targets))
    data))


(comment
  (suggested-edges llm-suggestions-2))



(defn init-cytoscape [container elements cy-el extra-data]
  (let [cy  (cytoscape (clj->js {:container container
                                 :elements (get-cyto-format-data-for-node {:nodes @elements
                                                                           :extra-data extra-data})
                                 :style  [{:selector "node"
                                           :style{:background-color "data(color)"
                                                  :border-width "data(bwidth)"
                                                  :border-style "data(bstyle)"
                                                  :border-color "data(bcolor)"
                                                  :shape "rectangle"
                                                  :height "120px"
                                                  :width "150px"
                                                  :label "data(label)"
                                                  :text-valign "center"
                                                  :text-halign "center"
                                                  :text-wrap "wrap"
                                                  :text-max-width "150px"
                                                  :text-justification "center"
                                                  :line-height 1.2
                                                  :font-size 10
                                                  :padding "5px"}}
                                          {:selector "edge"
                                           :style {:width 3
                                                   :line-color "data(color)"
                                                   :target-arrow-color "data(color)"
                                                   :target-arrow-shape "triangle"
                                                   :curve-style "bezier"
                                                   :label "data(label)"
                                                   :text-margin-y -10
                                                   :text-rotation "autorotate"
                                                   :font-size 12
                                                   :text-valign "top"
                                                   :color "data(color)" #_"#ff0000"}}]
                                 :layout default-layout}))]
    (reset! cy-el cy)))



(defn cytoscape-component [block-uid cy-el elements extra-data]
  (println "cytoscape component")
  (fn []
    [:div.cytoscape-container
     [:div
       {:class-name (str "cytoscape-view-" block-uid)
        :style {:width "1000px"
                :border "2px solid lightgrey"
                :height "1000px"}
        :ref (fn [el]
               (when el
                 (init-cytoscape el elements cy-el extra-data)))}]]))


(defn cytoscape-initial-component [block-uid message]
  (println "Initial cytoscape component")
  [:div
   {:class-name (str "cytoscape-main-" block-uid)}
   message
   "Please select the suggestions you want to visualise and then click **Connect to existing visualisation** button above"])


(defn cytoscape-main [block-uid dom-id]
  (let [parent-el (.getElementById js/document (str dom-id))]
    (.addEventListener parent-el "mousedown" (fn [e] (.stopPropagation e)))
    (rd/render [cytoscape-initial-component block-uid] parent-el)))
