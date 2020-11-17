(ns tsers.react
  (:require [clojure.string :as string]
            [goog.object :as gobj]
            ["react" :as reactjs]
            ["react-dom" :as react-dom]))

; cache for parsed hiccup tags and converted js props
(def ^:private tag-cache (js/Map.))
(def ^:private js-prop-cache
  (doto (js/Map.)
    (.set "class" "className")
    (.set "for" "htmlFor")
    (.set "charset" "charSet")))

;; Copied from reagent (originally copied from hiccup)
(def ^:private re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn- parse-tag [hiccup-tag]
  (let [tag-s (if (string? hiccup-tag) hiccup-tag (name hiccup-tag))]
    (if-let [cached-result (.get tag-cache tag-s)]
      cached-result
      (let [[tag id class] (->> tag-s (name) (re-matches re-tag) (next))
            _ (assert tag (str "Invalid tag: '" tag-s "'"))
            classes (some-> class (string/replace #"\." " "))
            result #js {:tag tag :id id :classes classes}]
        (.set tag-cache tag-s result)
        result))))

(defn- camelize-prop-key [s]
  (if-not (string/starts-with? s "data-")
    ;; JS interrop for SPEED
    (let [parts (.split s "-")
          n (alength parts)
          buf (js/Array. n)]
      (aset buf 0 (aget parts 0))
      (loop [i 1]
        (when (< i n)
          (as-> (aget parts i) p
                (str (.toUpperCase (.charAt p 0)) (subs p 1))
                (aset buf i p))
          (recur (inc i))))
      (.join buf ""))
    s))

(declare as-element jsfy-props)

(defn- primitive? [x]
  (or (boolean? x)
      (string? x)
      (number? x)
      (nil? x)
      (undefined? x)))

(defn- fq-name [kw]
  (str (if-let [ns (namespace kw)]
         (str ns "/"))
       (name kw)))

(defn- keyword->js-prop-name [k]
  (let [prop-s (name k)]
    (if-let [cached-result (.get js-prop-cache prop-s)]
      cached-result
      (let [res (camelize-prop-key prop-s)]
        (.set js-prop-cache prop-s res)
        res))))

(defn- jsfy-prop-key [k]
  (cond
    (keyword? k) (keyword->js-prop-name k)
    (string? k) k
    :else (throw (js/Error. (str "Invalid intrinsic property key" (pr-str k))))))

(defn- jsfy-prop-value [x]
  (cond
    (or (primitive? x)
        (fn? x)) x
    (keyword? x) (fq-name x)
    (symbol? x) (fq-name x)
    (map? x) (let [val #js {}]
               (doseq [[k v] x]
                 (unchecked-set val (jsfy-prop-key k) (jsfy-prop-value v)))
               val)
    (coll? x) (let [val #js []]
                (doseq [v x]
                  (.push val (jsfy-prop-value v)))
                val)
    (ifn? x) (fn [& args] (apply x args))
    (satisfies? IPrintWithWriter key) (pr-str key)
    :else x))

(defn- jsfy-class-name [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (symbol? x) (name x)
    (map? x) (->> (keep (fn [[k v]] (when v (jsfy-class-name k))) x)
                  (string/join " "))
    (coll? x) (->> (map jsfy-class-name x)
                   (string/join " "))
    :else (pr-str x)))

(defn- jsfy-element-props [props]
  (if (some? props)
    (let [js-props #js {}]
      (doseq [[k v] props]
        (case k
          :class (unchecked-set js-props "className" (jsfy-class-name v))
          :children nil
          (unchecked-set js-props (jsfy-prop-key k) (jsfy-prop-value v))))
      js-props)
    #js {}))

(defn- $ [type js-props cljs-children]
  (let [args #js [type js-props]]
    (doseq [child cljs-children]
      (.push args (as-element child)))
    (.apply reactjs/createElement nil args)))

(defn- create-fragment [props children]
  ($ reactjs/Fragment (jsfy-element-props props) children))

(defn- create-intrinsic-element [parsed-tag props children]
  (let [js-props (jsfy-element-props props)
        tag-name (unchecked-get parsed-tag "tag")
        id (unchecked-get parsed-tag "id")
        classes (unchecked-get parsed-tag "classes")]
    (when (some? id)
      (assert (nil? (unchecked-get js-props "id")) (str "Id defined twice for tag " tag-name))
      (unchecked-set js-props "id" id))
    (when (some? classes)
      (if-let [class-names-from-props (unchecked-get js-props "className")]
        (unchecked-set js-props "className" (str class-names-from-props " " classes))
        (unchecked-set js-props "className" classes)))
    ($ tag-name js-props children)))

(defn- unwrap-delegated-children [children]
  (when (seq children)
    (if (and (empty? (next children))
             (::children (meta (first children))))
      (first children)
      children)))

(defn- unwrap-props [wrapped-props]
  (let [children (some-> (unchecked-get wrapped-props "c")
                         (vary-meta assoc ::children true))
        props (or (unchecked-get wrapped-props "p") {})]
    (if children
      (assoc props :children children)
      props)))

(defn- wrap-props [props children]
  (if-some [key (:key props)]
    #js {:p props :c children :key (jsfy-prop-value key)}
    #js {:p props :c children}))

(def ^:private wrapper-key (js/Symbol "tsers$$wrapper"))
(def ^:private memo-wrapper-key (js/Symbol "tsers$$memo$$wrapper"))

(defn- display-name [comp]
  (or (.-displayName comp)
      (.-name comp)))

(defn- wrapper-component [component]
  (let [wrapper (fn [react-props]
                  (-> (unwrap-props react-props)
                      (component)
                      (as-element)))]
    (unchecked-set wrapper "displayName" (display-name component))
    wrapper))

(defn- memo-eq [prev-wrapped-props next-wrapped-props]
  (and (some? prev-wrapped-props)
       (some? next-wrapped-props)
       (= (unwrap-props prev-wrapped-props)
          (unwrap-props next-wrapped-props))))

(defn- create-component-element [component props children memo?]
  (let [type (if memo?
               (or (unchecked-get component memo-wrapper-key)
                   (let [wrapper (wrapper-component component)
                         memo (reactjs/memo wrapper memo-eq)]
                     (unchecked-set component memo-wrapper-key memo)
                     memo))
               (or (unchecked-get component wrapper-key)
                   (let [wrapper (wrapper-component component)]
                     (unchecked-set component wrapper-key wrapper)
                     wrapper)))]
    (reactjs/createElement type (wrap-props props children))))

(defn- hiccup->element [[type & [props & children :as props+children] :as hiccup]]
  (let [props (when (map? props) props)
        children (if props
                   (if (>= (count hiccup) 3)
                     (unwrap-delegated-children children)
                     (:children props))
                   (unwrap-delegated-children props+children))]
    (cond
      (= :<> type) (create-fragment props children)
      (or (keyword? type)
          (string? type)) (create-intrinsic-element (parse-tag type) props children)
      (or (fn? type)
          (ifn? type)) (create-component-element type props children (:memo (meta hiccup)))
      (some? (.-$$typeof type)) (create-component-element type props children false)
      :else (throw (js/Error. (str "Invalid hiccup tag type: " (type type)))))))

(defn- array-of-elements [xs]
  (let [elems #js []]
    (doseq [x xs] (.push elems (as-element x)))
    elems))

(defn- ref-atom [initial-value]
  (let [a (atom initial-value)]
    (assert (identical? js/undefined (.-current a)))
    ; for React ref interrop
    (js/Object.defineProperty
      a
      "current"
      #js {:get (fn [] @a)
           :set (fn [val] (reset! a val))})
    a))

(defn- jsfy-deps-array [deps]
  (when deps
    (let [js-deps #js []]
      (doseq [x deps]
        (->> (cond
               (keyword? x) (fq-name x)
               (symbol? x) (fq-name x)
               (uuid? x) (pr-str x)
               :else x)
             (.push js-deps)))
      js-deps)))

(def ^:private ErrorBoundary
  (let [proto #js {:componentDidCatch
                   (fn [err info]
                     (let [this (js-this)
                           on-error (.. this -props -onError)]
                       (on-error err info)
                       (.setState this #js {:latestError err})))
                   :shouldComponentUpdate
                   (fn [next-props _]
                     (not (identical? (.-props (js-this)) next-props)))
                   :render
                   (fn [] (.. (js-this) -props -children))}
        ctor (fn [props ctx]
               (let [this (js-this)]
                 (.call reactjs/Component this props ctx)))]
    (gobj/extend (.-prototype ctor) (.-prototype reactjs/Component) proto)
    (set! (.-displayName ctor) "ErrorBoundary")
    (set! (.. ctor -prototype -constructor) ctor)
    ctor))

(defn- wrap-eff [eff]
  (fn effect-wrapper []
    (let [cancel (eff)]
      (when (fn? cancel)
        cancel))))

;;

(defn as-element
  "Converts ClojureScript styled element (e.g. hiccup) to native
   React element. Normally you shouldn't use this from you CLJS
   codebase. Main use case is JavaScript library interoperability.

   ```clojure
   ;; the following lines are equivalent
   (as-element [:button {:disabled true} \"tsers\"])
   (create-element \"button\" #js {:disabled true} \"tsers\")

   ;; conversion is done for entire hiccup
   (def app-el
     (as-element [:div.app
                  [sidebar {:version 123 :title \"tsers\"]
                  [main {}]]))
   ```"
  [x]
  (cond
    (vector? x) (hiccup->element x)
    (primitive? x) x
    (seq? x) (array-of-elements x)
    (keyword? x) (fq-name x)
    (symbol? x) (fq-name x)
    (satisfies? IPrintWithWriter x) (pr-str x)
    :else x))

(def create-element
  "Native React.createElement. Does **not** perform any conversions,
   see [[as-element]] if you need to convert hiccup elements to
   native React elements."
  reactjs/createElement)

(defn render [element container]
  (react-dom/render (as-element element) container))

(defn use-state
  "Wrapper for React `useState` hook. Uses ClojureScript's value
   equality for detecting state changes, e.g.

   ```clojure
   (let [[state set-state] (use-state {:foo 12})
     ...
     (set-state {:foo 12}) ;; wont trigger state change
     ...)
   ```"
  [initial]
  (let [xs (reactjs/useState initial)
        state (aget xs 0)
        js-set-state (aget xs 1)
        set-state (or (unchecked-get js-set-state wrapper-key)
                      (let [f (fn [next]
                                (js-set-state
                                  (fn [v']
                                    (let [v (if (fn? next) (next v') next)]
                                      (if (= v v') v' v)))))]
                        (unchecked-set js-set-state wrapper-key f)
                        f))]
    [state set-state]))

(defn use-ref
  "Wrapper for React `useRef` hook. Returns ClojureScript atom instead of
   mutable JS object. Ref atom can be used as :ref in intrinsic elements."
  [initial]
  (let [ref (reactjs/useRef nil)]
    (or (unchecked-get ref "current")
        (let [a (ref-atom initial)]
          (unchecked-set ref "current" a)
          a))))

(defn use-effect
  "Wrapper for React `useEffect` hook. Uses reference equality for
   dependency change detection but treats the following types as
   'value' types: keywords, symbols, uuids. For rest (e.g. collections),
   use ClojureScript's [[hash]] function.

   ```clojure
   (let [[status set-status] (use-state :initial)
         _ (use-effect
             (fn []
               (let [title (case status
                             :initial \"Odottaa\"
                             :loading \"Ladataan..\"
                             \"...\")
                 (set! js/document -title title)))
             [status])
     ...)

   (let [[nums set-nums] (use-state [1 2 3 4])
         _ (use-effect
             (fn [] ...)
             [(hash nums)])]
     ...)
   ```"
  [eff deps]
  {:pre [(fn? eff)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useEffect (wrap-eff eff) (jsfy-deps-array deps)))

(defn use-layout-effect
  "Wrapper for React `useLayoutEffect` hook. Has same dependency
   semantics as [[use-effect]] hook."
  [eff deps]
  {:pre [(fn? eff)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useEffect (wrap-eff eff) (jsfy-deps-array deps)))

(defn use-memo
  "Wrapper for React `useMemo` hook. Has same dependency
   semantics as [[use-effect]] hook."
  [f deps]
  {:pre [(fn? f)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useMemo f (jsfy-deps-array deps)))

(defn use-callback
  "Wrapper for React `useCallback` hook. Has same dependency
   semantics as [[use-effect]] hook."
  [cb deps]
  {:pre [(fn? cb)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useCallback cb (jsfy-deps-array deps)))

(defn create-context
  "Creates a new React context that can be used with [[context-provider]]
   component and [[use-context]] hook."
  [default-value]
  (reactjs/createContext default-value))

(defn use-context
  "Wrapper for React `useContext` hook. Accepts context created with
   [[create-context]] function."
  [context]
  {:pre [(some? context)
         (some? (.-Provider context))]}
  (reactjs/useContext context))

(defn context-provider
  "Wrapper for React's context provider component.

  ```clojure
  (def user-ctx (create-context nil))

  [context-provider {:context user-ctx
                     :value   \"Matti\"}
   [application]]
  ```"
  [{:keys [context value children]}]
  {:pre [(some? context)
         (some? (.-Provider context))]}
  ($ (.-Provider context) #js {:value value} children))

(defn suspense
  "Wrapper for `React.Suspense` component

  ```clojure
  [suspense {:fallback [spinner]}
    ...]
  ```"
  [{:keys [fallback children]}]
  ($ reactjs/Suspense #js {:fallback (as-element fallback)} children))

(defn lazy
  "Wrapper for `React.lazy`. Expects promise to import a valid
   **cljs** react component (component that accepts persistent map
   as props and returns hiccup)."
  [loader-f]
  (let [type (reactjs/lazy
               (fn []
                 (.then (loader-f)
                        (fn [import]
                          (let [component (unchecked-get import "default")
                                wrapper (fn [js-props]
                                          (-> (unchecked-get js-props "p")
                                              (component)
                                              (as-element)))]
                            #js {:default wrapper})))))]
    (fn lazy-wrapper [props]
      ($ type #js {:p props} []))))

(defn error-boundary
  "Wrapper for `React.ErrorBoundary` component

  ```clojure
  (let [[error set-error] (use-state nil)]
    (if (nil? error)
      [error-boundary {:on-error set-error}
       [app]]
      [error-screen {:error error}]))
  ```"
  [{:keys [on-error children]}]
  {:pre [(fn? on-error)]}
  ($ ErrorBoundary #js {:onError on-error} children))