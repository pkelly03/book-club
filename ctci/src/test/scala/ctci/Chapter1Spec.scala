package ctci

import org.scalatest.{FunSpec, Matchers}



class Chapter1Spec extends FunSpec with Matchers {

  describe("chapter 1") {
    describe("exercise 1") {
      it ("Implement an algorithm to determine if a string has all unique characters What if you can not use additional data structures?") {

        Chapter1.Ben.exercise1_foldLeft("aaa") shouldBe false
        Chapter1.Ben.exercise1_foldLeft("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Chapter1.Ben.exercise1_foldLeft("zxcvbnmzxcvbnm") shouldBe false

        Chapter1.Ben.exercise1_groupByAndCollect("aaa") shouldBe false
        Chapter1.Ben.exercise1_groupByAndCollect("zxcvbnmzxcvbnm") shouldBe false

        Chapter1.Ben.exercise1_recursive("aaa") shouldBe false
        Chapter1.Ben.exercise1_recursive("zxcvbnmzxcvbnm") shouldBe false

        Chapter1.Paul.exercise1_usingCollect("aaa") shouldBe false
        Chapter1.Paul.exercise1_usingCollect("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Chapter1.Paul.exercise1_usingCollect("zxcvbnmzxcvbnm") shouldBe false

        Chapter1.Paul.exercise1_usingDistinct("aaa") shouldBe false
        Chapter1.Paul.exercise1_usingDistinct("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Chapter1.Paul.exercise1_usingDistinct("zxcvbnmzxcvbnm") shouldBe false

        Chapter1.Paul.exercise1_usingFoldLeftWithImmutableSet("aaa") shouldBe false
        Chapter1.Paul.exercise1_usingFoldLeftWithImmutableSet("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Chapter1.Paul.exercise1_usingFoldLeftWithImmutableSet("zxcvbnmzxcvbnm") shouldBe false

        Chapter1.Paul.exercise1_usingFoldLeftWithOneImmutableSet("aaa") shouldBe false
        Chapter1.Paul.exercise1_usingFoldLeftWithOneImmutableSet("zxcvbnmasdfghjklqwertyuiop") shouldBe true
        Chapter1.Paul.exercise1_usingFoldLeftWithOneImmutableSet("zxcvbnmzxcvbnm") shouldBe false

      }
    }
  }
}
