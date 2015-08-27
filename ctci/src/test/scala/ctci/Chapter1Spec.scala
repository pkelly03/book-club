package ctci

import org.scalatest.{FunSpec, Matchers}

class Chapter1Spec extends FunSpec with Matchers {

  describe("chapter 1") {
    describe("exercise 1") {
      it ("Implement an algorithm to determine if a string has all unique characters What if you can not use additional data structures?") {

        Chapter1.Ben.exercise1_1("aaa") shouldBe false
        Chapter1.Ben.exercise1_1("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Chapter1.Ben.exercise1_1("zxcvbnmzxcvbnm") shouldBe false


        Chapter1.Ben.exercise1_1b("aaa") shouldBe false
        Chapter1.Ben.exercise1_1b("zxcvbnmzxcvbnm") shouldBe false
      }
    }
  }
}
