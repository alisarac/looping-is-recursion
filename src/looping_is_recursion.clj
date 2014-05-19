(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [acc base exp]
                       (if (zero? exp)
                         acc
                         (recur (* acc base) base (- exp 1))))]
    (power-helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [elem a-seq]
                 (if (empty? a-seq)
                   elem
                   (recur (first a-seq) (rest a-seq)))
                 )]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
   (not= (count seq1) (count seq2)) false
   (and (empty? seq1) (empty? seq2)) true
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [n 0
         pred pred
         a-seq a-seq]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) n
     :else (recur (inc n) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         cnt 0
         a-seq a-seq]
    (cond
     (empty? a-seq) (/ acc cnt)
     :else (recur (+ acc (first a-seq)) (inc cnt) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [st #{}
         a-seq a-seq]
    (if (empty? a-seq)
      st
      (recur (toggle st (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [x1 0
         x2 1
         nt 2]
    (cond
     (= 1 n) 1
     (= 0 n) 0
     (= nt n) (+ x1 x2)
     :else (recur x2 (+ x2 x1) (inc nt)))))

(defn cut-at-repetition [a-seq]
  (loop [st #{}
         a-seq a-seq
         acc []]
    (cond
     (empty? a-seq) acc
     (st (first a-seq)) acc
     :else (recur (conj st (first a-seq)) (rest a-seq) (conj acc (first a-seq))))))

