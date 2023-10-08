(ns genetic.population
  (:require [clojure.core.reducers :as r]
            [genetic.DNA :as DNA]))

(defn pop:new [target mut-rat popl]
  {:DNAs popl
   :mating-pool []
   :generations 0
   :finished? false
   :target target
   :mutation-rate mut-rat
   :perfect-fit 1})

(defn construct [target mut-rat maxPopl]
  {:pre  [(string? target)
          (number? maxPopl)]
   :post [map?]}
  (->> (range maxPopl)
       (r/map (fn [_]
                (DNA/construct (count target))))
       (into [])
       (pop:new target mut-rat)))

(defn calc-fitness [popl]
  {:pre [(map? popl)]
   :post [map?]}
  (let [target-fit (partial DNA/calc-fitness
                            (:target popl))]
    (update popl :DNAs
            #(->>
              (r/map target-fit %)
              (into [])))))

(defn natural-selection [popl]
  {:pre [(map? popl)]
   :post [map?]}
  (let [DNAs (:DNAs popl)]
    (->> (for [dna DNAs
               :let [fitness (int (* (:fitness dna) 100))]
               _n (range fitness)]
           dna)
         (assoc popl :mating-pool))))

(defn parent->child [popl]
  (for [_dna (:DNAs popl)
        :let [mating-pool (:mating-pool popl)
              partner-a (rand-nth mating-pool)
              partner-b (rand-nth mating-pool)
              child (->> (DNA/crossover partner-a partner-b)
                         (DNA/mutate (:mutation-rate popl)))]]
    child))

(defn generate [popl]
  (-> (assoc popl :DNAs (parent->child popl))
      (update :generations inc)))

(defn evaluate [popl]
  #_(let [find-fit (fn [acc nex]
                     (if (> (:fitness acc) (:fitness nex))
                       (update acc :genes #(or % (:genes nex)))
                       nex))]
      (reduce find-fit {:fitness 0} popl))
  (let [best-dna (->> popl :DNAs (sort-by :fitness) last)]
    (assoc popl
           :finished? (= (int (:perfect-fit popl)) (int (:fitness best-dna)))
           :best (some-> best-dna (DNA/get-phrase))
           :fitness (:fitness best-dna))))

(defn average-fitness [popl]
  (/ (apply + (into [] (r/map :fitness (:DNAs popl))))
     (-> popl :DNAs count)))

(defn all-phrases [popl {:keys [min] :or {min 50}}]
  (->> (:DNAs popl)
       (r/map DNA/get-phrase)
       (r/take min)
       (into [])))