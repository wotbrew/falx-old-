(ns falx.template)

(defmulti template (fn [m template] template))

(def default-templates
  {:creature {:solid? true
              :creature? true}})

(defmethod template :default
  [m templ]
  (let [template (default-templates templ {})]
    (merge template m)))

(defn apply-template
  [m template]
  (if (seq? template)
    (reduce apply-template m template)
    (falx.template/template m template)))

