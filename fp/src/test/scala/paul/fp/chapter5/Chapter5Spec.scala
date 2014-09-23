package paul.fp.chapter5

import paul.fp.chapter5.Exercise1.{Stream, Empty}
import org.scalatest.{Matchers, WordSpec}
import paul.fp.chapter5.Exercise1.Stream

class Chapter5Spec extends WordSpec with Matchers {
  "Exercise 5.1 - toList" should {
    "implement toList" in {
      Stream().toList should be(Nil)
      Stream(1).toList should be(List(1))
      Stream(1, 2).toList should be(List(1, 2))
      Stream(1, 2, 3, 4, 5).toList should be(List(1, 2, 3, 4, 5))
    }
  }

  "Exercise 5.2a - drop" should {
    "implement drop(n)" in {
      Stream(1,2,3,4,5).drop(0).toList should be(Stream(1,2,3,4,5).toList)
      Stream(1,2,3,4,5).drop(2).toList should be(Stream(3,4,5).toList)
      Stream().drop(0) should be(Empty)
    }
  }

  "Exercise 5.2a - take" should {
    "implement take(n)" in {
      Stream(1, 2, 3, 4, 5).take(0).toList should be(Stream().toList)
      Stream(1, 2, 3, 4, 5).take(2).toList should be(Stream(1,2).toList)
    }
  }

  "Exercise 5.3 - take" should {
    "implement takeWhile(p: A => Boolean)" in {
      Stream(1, 2, 3, 4, 5).takeWhile(_ => true).toList should be(Stream(1,2,3,4,5).toList)
    }
  }


}