(ns hitype.util)

(defmacro def-with-macro
  "Defines a macro that calls the given body as a function.
  The function will always be the last argument.
  For example:

  (def-with-macro with-start-and-end-logging [name body-function]
    (println \"start\" name)
    (body-function)
    (println \"end\" name))"
  [macro-name & docstring-arguments-body]

  (let [[docstring arguments & body] (if (string? (first docstring-arguments-body))
                                       docstring-arguments-body
                                       (concat [nil]
                                               docstring-arguments-body))
        function-name (with-meta (symbol (str (name macro-name) "*"))
                        (meta macro-name))
        body-symbol (gensym "body")
        macro-arguments-without-body (vec (map #(symbol (str "argument-" %))
                                               (range (dec (count arguments)))))]

    `(do (defn ~function-name ~arguments ~@body)
         (defmacro ~macro-name ~(vec (concat macro-arguments-without-body
                                             ['& body-symbol]))
           (list ~function-name
                 ~@macro-arguments-without-body
                 (concat (list 'bound-fn [])
                         ~body-symbol)))
         (when ~docstring
           (alter-meta! (var ~macro-name) assoc :arglists (quote ~[(vec (concat (drop-last arguments)
                                                                                ['& 'body]))])
                        :doc ~docstring)))))
