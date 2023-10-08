(ns genetic.play
  (:require [genetic.population :as popl]))

(defn play! [& {:keys [phrase
                       mutation-rate
                       maximum-population]}]
  (let [popluation (popl/construct phrase mutation-rate maximum-population)]
    (loop [p popluation]
      (println (-> (select-keys p  [:best :generations :best-dna])
                   (update :best-dna :fitness)))
      (if (:finished? p)
        #_(or (:finished? p) (= (:generations p) 250))
        (select-keys p [:best
                        :best-dna
                        :generations
                        :target])
        (recur (->> p
                    popl/calc-fitness
                    popl/natural-selection
                    popl/generate
                    popl/calc-fitness
                    popl/evaluate))))))

(comment
  (try
    (play! :phrase "to be or not to be"
           :mutation-rate 0.01
           :maximum-population 400)
    (catch Exception e (println e)))
  :rcf)