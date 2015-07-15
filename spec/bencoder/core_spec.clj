(ns bencoder.core-spec
  (:require [speclj.core :refer :all]
            [bencoder.core :refer :all]))

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

(describe "decoder"

  (it "decodes an integer"
    (should= 42 (decoder "i42e")))

  (it "decodes a negative integer"
    (should= -42 (decoder "i-42e")))

  (it "decodes a string"
    (should= "abc" (decoder "3:abc")))

  (it "decodes another string"
    (should= "abcdefghij" (decoder "10:abcdefghij")))

  (it "decodes a string with other things after"
    (should= "abc" (decoder "3:abci42e")))
  
  (it "decodes a list with one element"
    (should= ["spam"] (decoder "l4:spame")))

  (it "decodes a list with two elements"
    (should= ["spam" 42] (decoder "l4:spami42ee")))

  (it "decodes a nested list"
    (should= [["foo" "bar"] "spam" 42] (decoder "ll3:foo3:bare4:spami42ee")))

  (it "decodes a map with one pair"
  	(should= {"foo" 42} (decoder "d3:fooi42ee"))))


(describe "Bencode"

  (it "encodes an integer"
    (should= "i42e" (bencode 42)))

  (it "encodes a negative integer"
    (should= "i-42e" (bencode -42)))

  (it "encodes a string"
    (should= "3:foo" (bencode "foo")))

  (it "encodes a list"
    (should= "l4:spami-42ee" (bencode ["spam" -42])))

  (it "encodes a map"
    (should= "d3:bar4:spam3:fooi42ee" (bencode {"bar" "spam", "foo" 42})))

  (it "encodes a map in lexocographical order"
    (should= "d3:bar4:spam3:fooi42ee" (bencode {"foo" 42, "bar" "spam"})))

  (it "encodes lists within maps"
    (should= "d1:ai1e3:fooli1ei2ei3e5:helloee" (bencode {"foo" [1 2 3 "hello"] "a" 1})))

  (it "encoces a maps within maps"
    (should= "d1:ai7e3:food1:ai3e1:bi4eee" (bencode {"foo" {"a" 3, "b" 4}, "a" 7}))))
