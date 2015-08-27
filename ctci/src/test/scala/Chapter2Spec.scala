package ctci

import org.scalatest.{FunSpec, WordSpec, Matchers, PropSpec}
import org.scalatest.prop._

class Chapter2Spec extends FunSpec with Matchers {

  describe("chapter 2") {
    describe("exercise 1") {
      it ("should return 10") {
        Chapter2.exercise1 should be(10)
      }
    }
  }
}
