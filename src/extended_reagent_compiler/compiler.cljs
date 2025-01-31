(ns extended-reagent-compiler.compiler
  (:require [goog.object :as gobj]
            [clojure.string :as string]
            [extended-reagent-compiler.protocols :as ep]
            [extended-reagent-compiler.utils :as extended.utils]
            [reagent.debug :refer-macros [dev? warn]]
            [reagent.impl.component :as comp]
            [reagent.impl.input :as input]
            [reagent.impl.protocols :as p]
            [reagent.impl.template :as t]
            [reagent.impl.util :as util :refer [named?]]))

(declare convert-prop-value)

(defn kv-conv [o k v]
  (doto o
    (gobj/set (t/cached-prop-name k) (convert-prop-value v))))

(defn convert-prop-value [x]
  (cond
    (util/js-val? x) x
    (named? x) (name x)
    (map? x) (reduce-kv kv-conv #js {} x)
    (vector? x) (extended.utils/map-array convert-prop-value x)
    (coll? x) (clj->js x)
    (ifn? x) (fn [& args]
               (apply x args))
    :else (clj->js x)))

(defn convert-props [props ^clj id-class compiler]
  (let [class (:class props)
        props (-> props
                  (cond-> class (assoc :class (util/class-names class)))
                  (t/set-id-class id-class))]
    (if (.-custom id-class)
      ;; TODO: consider adding support for vectors in custom elements
      (t/convert-custom-prop-value props)
      (if (ep/convert-props-in-vectors? compiler)
        (convert-prop-value props)
        (t/convert-prop-value props)))))

;;; This is used for html elements (:h1, :input) and also React component with :>/adapt-react-class
(defn native-element [parsed argv first ^p/Compiler compiler]
  (let [component   (.-tag parsed)
        props       (nth argv first nil)
        hasprops    (or (nil? props) (map? props))
        jsprops     (or (convert-props (if hasprops props) parsed compiler)
                        #js {})
        first-child (+ first (if hasprops 1 0))]
    (if (input/input-component? component)
      (let [;; Also read :key from props map, because
            ;; input wrapper will not place the key in useful place.
            react-key   (util/get-react-key props)
            input-class (or (.-reagentInput compiler)
                            (let [x (comp/create-class input/input-spec compiler)]
                              (set! (.-reagentInput compiler) x)
                              x))]
        (p/as-element
         compiler
         (with-meta [input-class argv component jsprops first-child compiler]
                    (merge (when react-key
                             {:key react-key})
                           (meta argv)))))
      (do
        (when-some [key (-> (meta argv) util/get-react-key)]
          (set! (.-key jsprops) key))
        (if (string? component)
          (p/make-element compiler argv (ep/get-component-from-lib compiler component) jsprops first-child)
          (p/make-element compiler argv component jsprops first-child))))))

(defn hiccup-element [v compiler]
  (let [tag (nth v 0 nil)
        n   (if (and (keyword? tag) (namespace tag))
              (.-fqn tag)
              (name tag))
        pos (.indexOf n ">")]
    (case pos
      -1 (native-element (p/parse-tag compiler n tag) v 1 compiler)
      0 (assert (= ">" n) (util/hiccup-err v (comp/comp-name) "Invalid Hiccup tag"))
      ;; Support extended hiccup syntax, i.e :div.bar>a.foo
      ;; Apply metadata (e.g. :key) to the outermost element.
      ;; Metadata is probably used only with sequeneces, and in that case
      ;; only the key of the outermost element matters.
      (recur (with-meta [(subs n 0 pos)
                         (assoc (with-meta v nil) 0 (subs n (inc pos)))]
                        (meta v))
             compiler))))

(defn vec-to-elem [v compiler fn-to-element]
  (when (nil? compiler)
    (js/console.error "vec-to-elem" (pr-str v)))
  (assert (pos? (count v)) (util/hiccup-err v (comp/comp-name) "Hiccup form should not be empty"))
  (let [tag (nth v 0 nil)]
    (assert (t/valid-tag? tag) (util/hiccup-err v (comp/comp-name) "Invalid Hiccup form"))
    (case tag
      :> (native-element (t/->HiccupTag (nth v 1 nil) nil nil nil) v 2 compiler)
      :r> (t/raw-element (nth v 1 nil) v compiler)
      :f> (t/function-element (nth v 1 nil) v 2 compiler)
      :<> (t/fragment-element v compiler)
      (cond
        (t/hiccup-tag? tag)
        (hiccup-element v compiler)

        (instance? t/NativeWrapper tag)
        (native-element tag v 1 compiler)

        :else (fn-to-element tag v compiler)))))


(defn as-element [this x fn-to-element]
  (cond
    (util/js-val? x)                x
    (vector? x)                     (vec-to-elem x this fn-to-element)
    (seq? x)                        (if (dev?)
                                      (t/expand-seq-check x this)
                                      (t/expand-seq x this))
    (named? x)                      (name x)
    (satisfies? IPrintWithWriter x) (pr-str x)
    :else                           x))

(defn parse-tag [hiccup-tag]
  ;; TODO: check if keywords can reach this place safely
  (let [[tag id className] (->> hiccup-tag str (re-matches t/re-tag) next)
        className (when-not (nil? className)
                    (string/replace className #"\." " "))]
    (assert tag (str "Invalid tag: '" hiccup-tag "'" (comp/comp-name)))
    (t/->HiccupTag tag
                   id
                   className
                   ;; Custom element names must contain hyphen
                   ;; https://www.w3.org/TR/custom-elements/#custom-elements-core-concepts
                   (not= -1 (.indexOf tag "-")))))

(defn cached-parse [this x _]
  (if-some [s (t/cache-get t/tag-name-cache x)]
    s
    (let [v (parse-tag x)]
      (gobj/set t/tag-name-cache x v)
      v)))

(defn get-component-from-lib [project-libs tag]
  (cond
    (string/includes? tag "/")
    (let [[lib component] (string/split tag #"/")]
      (gobj/get (gobj/get project-libs lib) (string/capitalize component)))

    (.hasOwnProperty project-libs "direct_usage")
    (gobj/get (.-direct_usage project-libs) (string/capitalize tag))

    :else
    tag))

(def get-component-from-lib-memo (memoize get-component-from-lib))

(defrecord ExtendedCompiler
  [id fn-to-element parse-fn
   ^js/Object project-libs convert-props-in-vectors?]
  p/Compiler
  (get-id [this] id)
  (parse-tag [this tag-name tag-value]
    (parse-fn this tag-name tag-value))
  (as-element [this x]
    (as-element this x fn-to-element))
  (make-element [this argv component jsprops first-child]
    (t/make-element this argv component jsprops first-child))

  ep/ExtendedCompiler
  (get-component-from-lib [this tag]
    (get-component-from-lib-memo project-libs tag))

  (convert-props-in-vectors? [this]
    convert-props-in-vectors?))

(defn create
  [{:keys [function-components project-libs convert-props-in-vectors?]
    :or   {project-libs {}}
    :as   opts}]
  (let [id            (gensym "reagent-extended-compiler")
        fn-to-element (if function-components
                        t/maybe-function-element
                        t/reag-element)
        parse-fn      (get opts :parse-tag cached-parse)]
    (->ExtendedCompiler
     id
     fn-to-element
     parse-fn
     (clj->js project-libs)
     convert-props-in-vectors?)))
