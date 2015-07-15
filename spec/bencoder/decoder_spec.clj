(ns bencoder.decoder-spec
  (:require [speclj.core :refer :all]
            [bencoder.decoder :refer :all]))

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
  	(should= {"foo" 42} (decoder "d3:fooi42ee")))

  (it "decodes a nested map"
    (should= {"foo" [1 2 3 "hello"] "a" 1} (decoder "d1:ai1e3:fooli1ei2ei3e5:helloee")))

  (it "decodes another nested map"
    (should= {"foo" {"a" 3, "b" 4}, "a" 7} (decoder "d1:ai7e3:food1:ai3e1:bi4eee")))

  (it "gives error if input is not properly encoded"
    (should-throw (decoder "zfgjk")))

  (it "gives error if input is not properly encoded within a nested map"
    (should-throw (decoder "d1:ai7e2:food1:ai3e1:bi4eee"))))