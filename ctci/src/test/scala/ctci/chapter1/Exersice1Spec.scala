package ctci.chapter1

import org.scalatest.{FunSpec, Matchers}



class Exersice1Spec extends FunSpec with Matchers {

  describe("chapter 1") {
    describe("exercise 1") {
      it ("Implement an algorithm to determine if a string has all unique characters What if you can not use additional data structures?") {

        Exercise1.Ben.exercise1_foldLeft("aaa") shouldBe false
        Exercise1.Ben.exercise1_foldLeft("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Exercise1.Ben.exercise1_foldLeft("zxcvbnmzxcvbnm") shouldBe false

        Exercise1.Ben.exercise1_groupByAndCollect("aaa") shouldBe false
        Exercise1.Ben.exercise1_groupByAndCollect("zxcvbnmzxcvbnm") shouldBe false

        Exercise1.Ben.exercise1_recursive("aaa") shouldBe false
        Exercise1.Ben.exercise1_recursive("zxcvbnmzxcvbnm") shouldBe false

        Exercise1.Paul.exercise1_usingCollect("aaa") shouldBe false
        Exercise1.Paul.exercise1_usingCollect("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Exercise1.Paul.exercise1_usingCollect("zxcvbnmzxcvbnm") shouldBe false

        Exercise1.Paul.exercise1_usingDistinct("aaa") shouldBe false
        Exercise1.Paul.exercise1_usingDistinct("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Exercise1.Paul.exercise1_usingDistinct("zxcvbnmzxcvbnm") shouldBe false

        Exercise1.Paul.exercise1_usingFoldLeftWithImmutableSet("aaa") shouldBe false
        Exercise1.Paul.exercise1_usingFoldLeftWithImmutableSet("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Exercise1.Paul.exercise1_usingFoldLeftWithImmutableSet("zxcvbnmzxcvbnm") shouldBe false

        Exercise1.Paul.exercise1_usingFoldLeftWithOneImmutableSet("aaa") shouldBe false
        Exercise1.Paul.exercise1_usingFoldLeftWithOneImmutableSet("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Exercise1.Paul.exercise1_usingFoldLeftWithOneImmutableSet("zxcvbnmzxcvbnm") shouldBe false

      }
    }
  }
}
