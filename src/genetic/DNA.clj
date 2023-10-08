(ns genetic.DNA
  (:require [clojure.core.reducers :as r]
            [clojure.string :as str]))

(defn rand-int> [from to]
  {:post [int?]}
  (let [r (rand-int (+ to 1))]
    (if (< r from) (rand-int> from to) r)))

(defn new-char-str []
  {:post [string?]}
  (let [ci (rand-int> 63 122)
        cr (case ci
             63 32
             64 46
             ci)]
    (some-> cr char str)))

(defn dna:new [genes]
  {:fitness 0.0
   :genes genes})

(defn construct [genec]
  {:pre [(number? genec)]
   :post [map?]}
  (->> (range genec)
       (r/map (fn [_] (new-char-str)))
       (into [])
       dna:new))

(defn get-phrase
  [dna]
  {:pre [(map? dna)]
   :post [string?]}
  (str/join (:genes dna)))

(defn calc-fitness [target dna]
  {:pre [(map? dna) (string? target)]
   :post [map?]}
  (let [score 0.0
        counter 0
        xf (fn [[s c] g]
             (if (= (str g) (-> target (nth c) str))
               [(inc s) (inc c)]
               [s (inc c)]))
        [scored] (into [] (r/reduce xf [score counter] (:genes dna)))]
    (assoc dna :fitness
           (float (/ scored (count target))))))

(defn crossover [A-dna B-dna]
  {:pre [(map? A-dna) (map? B-dna)]
   :post [map?]}
  (let [midpoint (rand-int (+ 1 (count (:genes A-dna))))]
    (->> (r/cat (into [] (r/take midpoint (:genes A-dna)))
                (into [] (r/drop midpoint (:genes B-dna))))
         r/flatten
         (into [])
         dna:new)))

(defn mutate [mutRate dna]
  {:pre [(number? mutRate) (map? dna)]
   :post [map?]}
  (let [gene:new (fn [gene]
                   (if (< (rand 1) mutRate)
                     (new-char-str)
                     gene))]
    (update dna :genes
            #(->> (r/map gene:new %)
                  (into [])))))


