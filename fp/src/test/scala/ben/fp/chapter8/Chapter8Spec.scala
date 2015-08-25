package ben.fp.chapter8

import ben.fp.chapter6.RNG.SimpleRNG
import ben.fp.chapter6.{RNG, State}
import ben.fp.chapter8.Chapter8.{Gen, PropV1}
import org.scalatest.{Matchers, WordSpec}

class Chapter8Spec extends WordSpec with Matchers {


  "8.3 Prop" should {

    def prop(value: Boolean) = new PropV1() {
      override def check: Boolean = value
    }

    "combine using && " in {

      (prop(true) && prop(true)).check shouldBe true
      (prop(true) && prop(false)).check shouldBe false
      (prop(false) && prop(false)).check shouldBe false
    }
  }

  "8.4" should {

    "choose within range" in {
      val range = 10
      val choose: Gen[Int] = Gen.choose(1, range)
      val withinRange = (1 to 1000) forall {
        is =>
          val (i, r) = choose.sample.run(SimpleRNG(is))
          i < range && i > 0
      }

      withinRange shouldBe true
    }
  }

  "8.5" should {

    "implement unit (Gen always returns the same state regardless of input)" in {
      val sample = Gen.unit(1).sample

      (1 to 1000) foreach {
        is =>
          val (i, rng) = sample.run(SimpleRNG(is))
          i shouldBe 1
      }
    }

    "implement boolean" in {

      (1 to 1000).toList.foldLeft(SimpleRNG(0) : RNG ) { case (rng, i) =>
          val (b, nextRng) = Gen.boolean.sample.run(rng)
          rng.nextInt._1 % 2 == 0 shouldBe b // same implementation as is in the boolean function itself
          nextRng
      }
    }

    "implement listOfN" in {

      val listGenerator : Gen[List[Int]] = Gen.listOfN(10, Gen.choose(1,10))
      val list = listGenerator.sample.run(SimpleRNG(0))._1

      list.size shouldBe 10
      list.foreach {
        i =>
          i > 0 shouldBe true
          i < 10 shouldBe true
      }
    }
  }

  "8.6" should {

    "implement flatMap to combine generators" in {

      val choose = Gen.choose(1, 10)
      val listOfN = choose.listOfN(Gen(State.unit(9)))
      listOfN.sample.run(SimpleRNG(0))._1.size shouldBe 9
    }
  }

  "8.7" should {

    "union" in {



    }
  }
}


























