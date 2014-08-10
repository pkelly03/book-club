package nuno.fp.chapter2

import nuno.fp.chapter2.Exercise5._
import org.scalatest.{Matchers, WordSpec}

class Exercise5Spec extends WordSpec with Matchers {
  "Exercise 5" should {
    "compose double with increment" in {
      compose[Int, Int, Int](_ + 1, _ * 2)(2) should be(5)
    }

    "compose identity with increment is the same as increment" in {
      compose[Int, Int, Int](_ + 1, identity)(2) should be(3)
    }

    "compose increment with identity is the same as increment" in {
      compose[Int, Int, Int](identity, _ + 1)(2) should be(3)
    }
  }
}