(ns extended-reagent-compiler.protocols)

(defprotocol ExtendedCompiler
  (get-component-from-lib [this tag])
  (convert-props-in-vectors? [this]))
