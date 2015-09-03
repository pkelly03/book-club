package ctci.chapter1

import org.scalatest.{FunSpec, Matchers}


class Exersice2Spec extends FunSpec with Matchers {

   describe("chapter 1") {
     describe("exercise 2") {
       it ("Write code to reverse a C-Style String") {

         import Exercise2._
         Ben.reverseString_usingReverse(cStyleString("abcdef")) shouldBe cStyleString("fedcba")
         Ben.reverseString_usingRecurssion(cStyleString("abcdef")) shouldBe cStyleString("fedcba")
       }
     }
   }
 }
