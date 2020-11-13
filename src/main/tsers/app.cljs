(ns tsers.app
  (:require [tsers.react :as react]))

(def theme-ctx (react/create-context nil))

(defn counter []
  (let [[count set-count] (react/use-state 0)
        theme (react/use-context theme-ctx)
        _ (react/use-effect
            (fn []
              (when (pos? count)
                (cond
                  (zero? (mod count 15)) (js/alert "lolbal")
                  (zero? (mod count 5)) (js/alert "bal")
                  (zero? (mod count 3)) (js/alert "lol"))))
            [count])]
    [:div
     [:div "Counter: " count]
     [:div
      [:button {:on-click #(set-count dec)}
       "-"]
      [:button {:on-click #(set-count inc)}
       "+"]]
     [:div "Theme is: " theme]]))

(defn todo-item [{:keys [todo]}]
  (println "RENDER TODO" (:id todo))
  [:li (:text todo)])

(defn todos []
  (let [[todos set-todos] (react/use-state [])
        [input set-input] (react/use-state "")
        ids (react/use-ref 0)]
    [:div
     [:div
      [:input {:placeholder "New todo"
               :value       input
               :on-change   #(set-input (.. % -target -value))}]
      [:button {:on-click (fn []
                            (set-todos #(conj % {:id   (swap! ids inc)
                                                 :text input}))
                            (set-input ""))
                :disabled (empty? input)}
       "Add"]
      [:ul
       (for [{:keys [id] :as todo} todos]
         ^:memo
         [todo-item {:key id :todo todo}])]]]))

(defn theme-provider [{:keys [theme children]}]
  [react/context-provider
   {:context theme-ctx
    :value   theme}
   [:section
    children]])

(defn app []
  [theme-provider {:theme "jaffa"}
   [:div
    [:h1 "Hello vanilla react"]
    [counter]
    [:hr]
    [todos]]])

(def ^:private container
  (js/document.getElementById "app"))

(defn main []
  (react/render [app] container))

(defn ^:dev/after-load -after-reload []
  (react/render nil container)
  (react/render [app] container))
