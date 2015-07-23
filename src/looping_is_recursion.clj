(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [coll]
                 (if (empty? (rest coll))
                   (first coll)
                   (recur (rest coll))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                  (and (empty? a-seq) (empty? b-seq)) true
                  (or (empty? a-seq) (empty? b-seq)) false
                  (not (= (first a-seq) (first b-seq))) false
                  :else (recur (rest a-seq) (rest b-seq))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [coll a-seq
         n 0]
    (cond
     (empty? coll) nil
     (pred (first coll)) n
     :else (recur (rest coll) (inc n)))))

(defn avg [a-seq]
  (loop [coll a-seq
         sum 0]
    (if (empty? coll)
      (/ sum (count a-seq))
      (recur (rest coll) (+ sum (first coll))))))

(defn parity [a-seq]
  (loop [freqs (frequencies a-seq)
         parities '#{}]
    (if (empty? freqs)
      parities
      (if (= (mod (second (first freqs)) 2) 0)
        (recur (rest freqs) parities)
        (recur (rest freqs) (conj parities (first (first freqs))))))))

(defn fast-fibo [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (loop [i n
                n-1 1
                n-2 0]
           (if (== i 2)
             (+ n-1 n-2)
             (recur (dec i) (+ n-1 n-2) n-1)))))

(defn cut-at-repetition [a-seq]
  (loop [index 0
         coll a-seq
         id-set '#{}]
    (cond
     (empty? coll) a-seq
     (contains? id-set (first coll)) (take index a-seq)
     :else (recur (inc index) (rest coll) (conj id-set (first coll))))))
