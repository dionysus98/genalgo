(ns genetic.core
  (:require [genetic.population :as popl]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn init! [& {:keys [phrase
                       mutation-rate
                       maximum-population
                       verbose]}]
  (try
    (let [popluation (popl/construct phrase mutation-rate maximum-population)]
      (println "Target Received      : " phrase)
      (println "Mutation Rate        : " mutation-rate)
      (println "Maximum Population   : " maximum-population)
      (println "Verbose Execution    : " (if verbose "yes" "no"))
      (println "Built Population.")
      (println "Starting Execution.. [to get detailed info about the process pass -v option]")
      (loop [p popluation]
        (when verbose
          (println (-> (select-keys p [:best
                                       :generations
                                       :fitness
                                       :mating-pool])
                       (update :mating-pool count))))
        (if (:finished? p)
          (println (select-keys p [:best
                                   :fitness
                                   :generations
                                   :target]))
          (recur (->> p
                      popl/calc-fitness
                      popl/natural-selection
                      popl/generate
                      popl/calc-fitness
                      popl/evaluate))))
      (println "Finished Execution.."))
    :ok
    (catch Exception e
      (println e)
      :error)))

(def cli-options
  [["-p" "--phrase PHRASE" "Target Phrase."
    :default "O Grand love"
    :validate [#(and  (string? %) (not-empty %))
               "Should not be an empty phrase"]]
   ["-m" "--mutation-rate MUT_RATE" "Mutation Rate."
    :default 0.01
    :parse-fn #(parse-double %)
    :validate [#(<= 0.0 % 1.0) "Must be a number between 0 and 1"]]
   ["-n" "--maximum-population MAX_POP_RATE" "Maximum amount of Neural Networks to be operated."
    :default 200
    :parse-fn #(-> % parse-double int)
    :validate [#(not (zero? %)) "Must be a number greater than 0"]]
   ["-v" "--verbose" "Detailed info about the algorithm execution"]
   ["-d" "--default" "Run the algorithm with default set of values."]
   ["-h" "--help"]])

(defn -main
  [& args]
  (let [{opts :options
         sum  :summary
         err :errors} (parse-opts args cli-options)]
    (cond
      err (println (str err "\n" sum))
      (:help opts) (println sum)
      (:default opts) (init! opts)
      :else (init! opts)))) 