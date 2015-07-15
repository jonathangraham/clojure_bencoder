(ns bencoder.core-spec
  (:require [speclj.core :refer :all]
            [bencoder.core :refer :all]))

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
    (should= "d1:ai7e3:food1:ai3e1:bi4eee" (bencode {"foo" {"a" 3, "b" 4}, "a" 7})))

  (it "doesn't encode a float"
    (should-throw (bencode 1.0))))