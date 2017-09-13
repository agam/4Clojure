(ns 4clojure.core)

(defn my-nth [seq n]
  (if (= n 0)
    (first seq)
    (recur (rest seq) (- n 1))))


(defn my-reverse [coll]
  (loop [s '()
         remainder coll]
    (if (seq remainder)
      (recur (conj s (first remainder)) (rest remainder))
      s)))


(defn my-sum [coll]
  (reduce + coll))

(defn find-odd [coll]
  (filter odd? coll))

(defn my-fib [n]
  (cond
    (= n 0) '()
    (= n 1) '(1)
    (= n 2) '(1 1)
    :else (let [prior (my-fib (- n 1))
                last-two (take-last 2 prior)]
            (conj (vec prior)
                  (+ (first last-two)
                     (second last-two))))))


(defn palindrome? [coll]
  (= (seq coll) (reverse coll)))


(defn my-flatten [coll]
  (cond
    (nil? coll) '()
    
    (or (list? coll) (vector? coll))
    ;; Pull out nested lists
    (apply concat (map my-flatten coll))
    
    :else (seq (list coll))))


(defn get-uppers [strarg]
  (apply str
         (filter #(Character/isUpperCase %) strarg)))

(defn nil-key?
  [k m]
  (if (contains? m k)
    (nil? (m k))
    false))

(defn defaults-fn
  [v ks]
  (apply hash-map (interleave ks (repeat v))))

(defn my-max
  "My very own max function"
  [& args]
  (cond
    (= 0 (count args)) nil
    (= 1 (count args)) (first args)

    :else
    (reduce (fn [x y]
              (if (> x y) x y))
            (first args)
            (rest args))))

(defn dup-seq
  "Make a copy of every element within the sequence passed in."
  [s]
  (interleave s s))

(defn my-range
  "Return list of all integers from 'start' upto one less than 'end'"
  [start end]
  (reverse
   (loop [r '()
          n start]
     (if (= n end)
       r
       (recur (conj r n) (inc n))))))

(defn compress
  [seq]
  (let [unique-pairs
        (filter (fn [[x y]]
                  (not= x y))
                (partition 2 1 seq))]
    (conj (vec (map #(first %) unique-pairs))
          (second (last unique-pairs)))))

(defn factorial
  [n]
  (loop [calc 1
         counter n]
    (if (= counter 1)
      calc
      (recur (* calc counter) (dec counter)))))

(defn my-interleave
  [arg1 arg2]
  (reverse
   (loop [result '()
          s1 arg1
          s2 arg2]
     (let [n1 (first s1)
           n2 (first s2)]
       (if (or (nil? n1)
               (nil? n2))
         result
         (recur (conj result
                      n1 n2)
                (rest s1)
                (rest s2)))))))

(defn my-replicate
  [coll n]
  (mapcat
    #(repeat n %)
    coll))

(defn my-interpose
  [sep coll]
  (reverse
   (loop [remaining (seq coll)
          return (list (first coll))]
     (if (next remaining)
       (recur (rest remaining)
              (conj return sep (second remaining)))
       return))))

(defn old-pack-sequence
  [coll]
  (if (= 0 (count coll))
    '()
    (loop [last-element (first coll)
           remaining coll
           current-count 0
           return '()]
      (let [next-element (first remaining)]
        (if next-element
          (if (= next-element last-element)
            
            (recur last-element
                   (rest remaining)
                   (inc current-count)
                   return)

            (recur next-element
                   (rest remaining)
                   1
                   (conj return
                         (list last-element current-count))))

          (conj return (list last-element current-count)))))))


(defn pack-sequence
  [coll]
  (reverse 
   (loop [last-element (first coll)
          remaining (rest coll)
          subsequence '()
          return '()]
     (let [next-element (first remaining)
           next-pack (conj subsequence last-element)]

       (if next-element

         (if (= next-element last-element)

           (recur
            last-element
            (rest remaining)
            (conj subsequence last-element)
            return)

           (recur
            next-element
            (rest remaining)
            '()
            (conj return next-pack)))

         (conj return next-pack))))))

(defn drop-nth
  [coll n]
  (reverse
   (loop [remaining coll
          current 1
          return '()]

     (if (zero? (count remaining))

       return

       (if (= current n)

         (recur (rest remaining)
                1
                return)

         (recur (rest remaining)
                (inc current)
                (conj return (first remaining))))))))

(defn my-split-at
  [n coll]
  [(take n coll) (drop n coll)])

(defn some-true?
  [& args]
  (let [has-element (fn [elem coll]
                      (->> args
                           (filter #(= elem %))
                           (count)
                           (< 0)))]
  (and (has-element true args)
       (has-element false args))))

(defn make-map
  [ks vs]
  (loop [rk ks
         rv vs
         m {}]
    (if (and (first rk) (first rv))

      (recur
       (rest rk) (rest rv)
       (assoc m (first rk) (first rv)))

      m)))

(defn gcd
  [a b]
  (if (= a b)
    a
    (if (< a b)
      (recur a (- b a))
      (recur b (- a b)))))

(defn intersect
  [s1 s2]
  (loop [srest s2
         result #{}]
    (let [selem (first srest)]

      (if selem
        (if (contains? s1 selem)
          
          (recur (rest srest)
                 (conj result selem))

          (recur (rest srest)
                 result))
        
        result))))

(defn power-closure [n]
  ; Return a function (f x) that computes x^n
  (fn [x]
    (int (Math/pow x n))))

(defn my-comp [lt x y]
  (cond
    (lt x y) :lt
    (lt y x) :gt
    :else :eq))

(defn my-iterate [f x]
  (let [next-val (f x)]
    (cons x
          (lazy-seq
           (my-iterate f next-val)))))

(defn cartesian-product [s1 s2]
  (set (for [x s1
             y s2]
         [x y])))

(defn product-digits [x y]
  (let [product (* x y)]
    (loop [return []
           num product]
      (if (= num 0)
        (reverse return)

        (let [next-digit (rem num 10)]
          (recur (conj return next-digit)
                 (quot num 10)))))))

(defn my-group-by [f coll]
  (loop [result {}
         s coll]
    
    (let [next-element (first s)]
      
      (if next-element

        (let [val (f next-element)
              existing-elements (get result val)]
          
          (recur (conj result
                       [val (conj (vec existing-elements)
                                  next-element)])
                 (rest s)))

        result))))

(defn symmetric-difference [s1 s2]
  (clojure.set/union
   (clojure.set/difference s1 s2)
   (clojure.set/difference s2 s1)))

(defn dot-product [v1 v2]
  (apply +
         (map * v1 v2)))

(defn read-binary [s]
  (let [c->b (fn [c]
               (- (int c) (int \0)))
        digits (map c->b (seq s))
        pow-2 (fn [n]
                (reverse (take n
                               (iterate #(* 2 %) 1))))]

    (apply +
           (map *
                digits
                (pow-2 (count digits))))))

(defn brain-teaser [v]
  (let [x v]
    (and (= (class x) x)
         x)))

(defn math-eval [& args]
  (let [eval-three (fn [arg1 op arg2]
                     (op arg1 arg2))

        param-sets (partition 2 (rest args))

        first-arg (first args)]

    (reduce (fn [arg1  [op arg2 :as arg-pair]]
              (op arg1 arg2))

            first-arg param-sets)))

(defn elem-indices [coll]
  (map-indexed (fn [n elem]
                 (vec (list elem n)))
               coll))

(defn pascal-row [n]
  (let [update-elements
        (fn [old-row]
          (loop [new-row (vec
                          (concat [1]
                                  (take (dec (count old-row))
                                        (repeat 0))
                                  [1]))
                 index 1]
            (if (= index (dec (count new-row)))
              new-row

              (recur
               (assoc new-row index
                      (+ (old-row (dec index))
                         (old-row index)))
               (inc index)))))]
    
    (cond 
          (= n 1)  [1]
          (= n 2)  [1 1]
          
          :else
          (update-elements (vec (pascal-row (dec n)))))))

(defn my-map
  [f coll]
  (when-let [s (seq coll)]
    (lazy-seq
     (cons (f (first s))
           (my-map f (rest s))))))

(defn is-tree?
  [coll]

  (let [is-leaf? (fn [v]
                   (not (coll? v)))]
    
    (if (and (coll? coll)
             (= (count coll) 3))

      (let [root (nth coll 0)
            left-child (nth coll 1)
            right-child (nth coll 2)]

        (true? (and (is-leaf? root)

             (or (is-tree? left-child)
                 (nil? left-child))

             (or (is-tree? right-child)
                 (nil? right-child)))))


      false)))


(defn prob-120  ; Couldn't think of a sane name!
  [args]

  (let [digits
        (fn [x]
          (loop [result []
                 num x]
            (if (zero? num)
              result
              (recur (conj result (rem num 10))
                     (quot num 10)))))

        sum-squares
        (fn [x]
          (apply + (map #(* % %) (digits x))))]
    
    (count
     (filter pos? 
             (map (fn [x y] (- y x))
                  args
                  (map sum-squares args))))))
           
(defn card-parser
  [card-str]
  (let [parse-suit
        (fn [suit-char]
          (case suit-char
            \D :diamond
            \H :heart
            \C :club
            \S :spade))

        char= (fn [c1 c2]
                (== 0 (compare c1 c2)))
        
        parse-rank
        (fn [rank-char]
          (cond
            (char= rank-char \T) 8
            (char= rank-char \J) 9
            (char= rank-char \Q) 10
            (char= rank-char \K) 11
            (char= rank-char \A) 12
            :else (- (int rank-char) (int \2))))]
       
    {:suit (parse-suit (first card-str))
     :rank (parse-rank (second card-str))}))

(defn lcm [& args]
  (let [gcd (fn  [a b]
              (if (= a b)
                a
                (if (< a b)
                  (recur a (- b a))
                  (recur b (- a b)))))
        
        lcm-2 (fn [x y]
                (/ (* x y)
                   (gcd x y)))]

    (reduce lcm-2 args)))

        
(defn pascal-trapezoid [start-vec]

  (let [next-vec
        (fn [old-row]
          (loop [new-row (vec
                          (concat [(first old-row)]
                                  (take (dec (count old-row))
                                        (repeat 0))
                                  [(last old-row)]))
                 index 1]

            (if (= index (dec (count new-row)))
              new-row

              (recur
               (assoc new-row index
                      (+' (old-row (dec index))
                          (old-row index)))
               (inc index)))))]
  
  (iterate next-vec start-vec)))

(defn symmetric-tree? [& [[r lt rt] :as coll]]

  (let [mirror?
        (fn mirror? [t1 t2]
          (if (coll? t1)
            (let [[r1 lt1 rt1] t1
                  [r2 lt2 rt2] t2]
            (and (= r1 r2)
                 (mirror? lt1 rt2)
                 (mirror? rt1 lt2)))

            (= t1 t2)))]
    
    (mirror? lt rt)))

(defn flatten-map [m]
  (reduce merge
          (for [[k mv] m]
            (into {}
                  (for [[k' v'] mv]
                    [[k k'] v'])))))


(defn make-products [scoll]
  (reduce concat
          (for [s1 scoll]
            (for [s2 (disj scoll s1)]
              [s1 s2]))))


(defn disjoint? [s1 s2]
  (empty? (clojure.set/intersection s1 s2)))

(defn disjoint-sets [scoll]
  (let [disjoint? (fn [s1 s2]
                    (empty? (clojure.set/intersection s1 s2)))

        make-products (fn [scoll]
                        (reduce concat
                                (for [s1 scoll]
                                  (for [s2 (disj scoll s1)]
                                    [s1 s2]))))]

    (reduce #(and %1 %2) (map (fn [[x y]] (disjoint? x y))
                              (make-products scoll)))))

