package ctci.chapter1

import org.scalatest.{FunSpec, Matchers}


class Exersice3Spec extends FunSpec with Matchers {

   describe("chapter 1") {
     describe("exercise 3") {
       it ("Remove the duplicate characters in a string without using any additional buffer") {

         import Exercise3._
         Ben.removeDuplicates_usingRecursion("abcdefab") shouldBe "abcdef"
         Ben.removeDuplicates_usingRecursion("aaaaaa") shouldBe "a"

         Ben.removeDuplicates_usingDistinct("abcdefab") shouldBe "abcdef"
         Ben.removeDuplicates_usingDistinct("aaaaaa") shouldBe "a"
       }
     }
   }
 }
