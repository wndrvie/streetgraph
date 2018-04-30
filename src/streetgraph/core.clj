(ns streetgraph.core
  (:require
    [streetgraph.task2 :refer :all])
  (:gen-class))

(defn -main
  "What else to say?"
  [& args]
  (time (run-task-2 10 20)))
