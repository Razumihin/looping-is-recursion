(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k] 
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq n] 
                 (if (zero? n)
                   (first a-seq)
                   (recur (rest a-seq) (dec n))))]
    (helper a-seq (- (count a-seq) 1))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2] 
                 (if (and (empty? seq1) (empty? seq2))
                   true
                   (if (== (first seq1) (first seq2))
                     (recur (rest seq1) (rest seq2))
                     false)))]
    (if (== (count seq1) (count seq2))
      (helper seq1 seq2)
      false)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         p pred 
         s a-seq]
    (if (empty? s)
      nil
      (if (pred (first s))
        acc
        (recur (inc acc) p (rest s))))))

(defn avg [a-seq]
  (loop [sum 0 
         div 0
         seq1 a-seq]
    (if (empty? seq1)
      (/ sum div)
      (recur (+ sum (first seq1)) (inc div) (rest seq1)))))

(defn toggleElem [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [paritySeq #{}
         seq1 a-seq]
    (if (empty? seq1)
      paritySeq
      (recur (toggleElem paritySeq (first seq1)) (rest seq1)))))

(defn fast-fibo [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    (== n 2) 1
  :else
    (loop [n-1 1
           n-2 1
           c (- n 3)]
      (if (== c 0) 
        (+ n-2 n-1)
        (recur (+ n-2 n-1) n-1 (dec c))
        ))))

(defn cut-at-repetition [a-seq]
  (loop [cutSeq #{}
         a-seq a-seq]
    (if (or (empty? a-seq) (contains? cutSeq (first a-seq)))
      cutSeq 
      (recur (conj cutSeq (first a-seq)) (rest a-seq)))))

