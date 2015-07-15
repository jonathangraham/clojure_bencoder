(ns bencoder.core)

(declare decoder)

(defn bencode [s]
  (cond (integer? s) (str "i" s "e")
        (string? s) (str (count s) ":" s)
        (sequential? s) (str "l" (apply str (mapcat bencode s)) "e")
        :else (str "d" (apply str (mapcat #(str (bencode (key %))
                                                (bencode (val %))) (sort-by key s))) "e")))

(defn encoded-integer? [s]
  (= \i (first s)))

(defn encoded-string? [s]
  (integer? (read-string (subs s 0 1))))

(defn encoded-list? [s]
  (= \l (first s)))

(defn encoded-map? [s]
  (= \d (first s)))

(defn find-integer [s]
  (Integer. (re-find #"\d+" s)))

(defn decode-integer [s]
  (let [abs (find-integer s)]
  	(if (= \- (second s))
  		(* -1 abs)
  		abs)))

(defn decode-string [s]
  (let [string-length (find-integer s)
        offset (inc (count (str string-length)))]
    (subs s offset (+ offset string-length))))

(defn decode-list [s]
  (loop [remaining (subs s 1) coll []]
  	(if (= \e (first remaining))
  		coll
  		(let [x (decoder remaining)
  			l (count (bencode x))]
  		(recur (subs remaining l) (conj coll x))))))

(defn decode-map [s]
	(apply hash-map (decode-list s)))

(defn decoder [s]
  (let [c (count s)]
    (cond (encoded-integer? s) (decode-integer s)
          (encoded-string? s) (decode-string s)
          (encoded-list? s) (decode-list s)
          (encoded-map? s) (decode-map s)
          :else "Error. The bencode only works with integers, strings, lists and maps." )))
