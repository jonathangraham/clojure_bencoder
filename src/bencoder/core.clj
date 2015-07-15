(ns bencoder.core)

(derive java.util.Map ::map)
(derive java.util.Collection ::collection)
(derive java.lang.Long ::integer)
(derive java.lang.String ::string)

(defmulti bencode class)
		
	(prefer-method bencode ::map ::collection)

	(defmethod bencode ::integer [s]
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