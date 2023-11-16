(ns parka.portal.tree-viewer
  (:require
   [parka.portal.zip :as z]
   [parka.reflect :as reflect]
   [portal.ui.api :as p]
   [portal.ui.inspector :as ins]
   [reagent.core :as r]))

(defn- left [zip]
  (or (z/left zip) (z/prev zip)))

(defn- step-over [zip]
  (or (z/right zip) (z/next zip)))

(defn- step-in [zip]
  (or (z/down zip) (z/next zip)))

(defn- step-out [zip]
  (or (z/up zip) zip))

(defn parse-tree-viewer []
  (let [state (r/atom nil)]
    (fn [tree]
      (when-not @state
        (reset! state (z/zipper #_branch?   map?
                                #_children  :children
                                #_make-node #(assoc %1 :children %2)
                                tree)))
      (let [{:keys [expr]
             {:keys [input pos]} :state
             [res state' value] :result
             :as parsed} (z/node @state)
            ui
        [:<>
         [:nav
          [:button {:on-click #(swap! state left)}      "Back"]
          [:button {:on-click #(swap! state step-out)}  "Up"]
          [:button {:on-click #(swap! state step-in)}   "In"]
          [:button {:on-click #(swap! state step-over)} "Over"]]
         [:div.tree
          [:h3 "Tree"]
          (for [prev (z/path @state)]
            [:div (-> prev :expr reflect/print-expr pr-str)])]

         [:div.parser
          [:label "Parser: "]
          (pr-str (reflect/print-expr expr))]

         (let [prev-start                   (max 0 (- pos 20))
               consume-end                  (if (= res :success)
                                              (:pos state')
                                              pos)
               post-end                     (min (count input) (+ consume-end 20))
               before                       (subs input prev-start pos)
               consumed                     (subs input pos consume-end)
               after                        (when (< consume-end (count input))
                                              (subs input consume-end post-end))]
           [:div.input
            [:label "Input: "]
            [:span {:style #js {"color" "#888"}}             before]
            [:span {:style #js {"color"       "#fff"
                                "font-weight" "bold"}}       consumed]
            [:span                                           after]])
         [:div.result
          [:pre (pr-str value)]]
         #_[:div.debug
          (ins/inspector parsed)]]]
        (js/console.log (pr-str ui))
        ui))))

(p/register-viewer!
  {:name      :parka.dynamic/parse-tree
   :predicate :parka.dynamic/parse-tree
   :component parse-tree-viewer})
