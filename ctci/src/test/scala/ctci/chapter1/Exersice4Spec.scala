package ctci.chapter1

import org.scalatest.{FunSpec, Matchers}


class Exersice4Spec extends FunSpec with Matchers {

   describe("chapter 1") {
     describe("exercise 4") {
       it ("Write a method to decide if two strings are anagrams or not") {

         import Exercise4._
         Ben.areAnagrams_usingSort("abba", "baba") shouldBe true
         Ben.areAnagrams_usingSort("a", "a") shouldBe true
         Ben.areAnagrams_usingSort("these are the same", "same these are the") shouldBe true
         Ben.areAnagrams_usingSort("abba", "ababa") shouldBe false

         Ben.areAnagrams_usingGroupBy("abba", "baba") shouldBe true
         Ben.areAnagrams_usingGroupBy("a", "a") shouldBe true
         Ben.areAnagrams_usingGroupBy("these are the same", "same these are the") shouldBe true
         Ben.areAnagrams_usingGroupBy("abba", "ababa") shouldBe false

         Ben.areAnagrams_usingRecursion("abba", "baba") shouldBe true
         Ben.areAnagrams_usingGroupBy("a", "a") shouldBe true
         Ben.areAnagrams_usingGroupBy("these are the same", "same these are the") shouldBe true
         Ben.areAnagrams_usingRecursion("abba", "ababa") shouldBe false
       }
     }
   }
 }
