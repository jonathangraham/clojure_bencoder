(ns bencoder.core)

(derive java.util.Map ::map)
(derive java.util.Collection ::collection)
(derive java.lang.Long ::integer)
(derive java.lang.String ::string)



(defmulti bencode class)
		
	(prefer-method bencode ::map ::collection)

	(defmethod bencode ::number [s]
		(str "i" s "e"))

	(defmethod bencode ::string [s]
		(str (count s) ":" s))

	(defmethod bencode ::collection [s]
		(str "l" (apply str (mapcat bencode s)) "e"))

	(defmethod bencode ::map [s]
		(str "d" 
			(apply str (mapcat #(str (bencode (key %)) (bencode (val %))) (sort-by key s))) 
			"e"))

	(defmethod bencode :default [s]
		(throw (Exception. "The bencode only works with integers, strings, lists and maps.")))




(declare decoder)

(defn find-integer [s]
  (Long. (re-find #"\d+" s)))

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

(defmulti decoder (fn [s] 
	(let [s1 (str (first s))]
		(if (< 0 (count (re-find #"\d+" s1)))
		"1"
		s1))))

	(defmethod decoder "i" [s]
		(decode-integer s))

	(defmethod decoder "1" [s]
		(decode-string s))

	(defmethod decoder "l" [s]
		(decode-list s))

	(defmethod decoder "d" [s]
		(decode-map s))

	(defmethod decoder :default [s]
		(throw (Exception. "The input is not encoded according to the bencoder requirements")))