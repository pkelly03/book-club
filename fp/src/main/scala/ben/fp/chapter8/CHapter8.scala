package ben.fp.chapter8

import ben.fp.chapter6.{RNG, State}
import ben.fp.chapter8.Chapter8.Prop.{Result, TestCases}

object Chapter8 {

  trait PropV1 {

    def &&(that: PropV1): PropV1 = {
      val c = this.check
      new PropV1 {
        override def check: Boolean = c && that.check
      }
    }

    def check: Boolean
  }


  object Prop {
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified = true
    }

    case object Proved extends Result {
      def isFalsified = false
    }
  }

  case class Prop(run: TestCases => Result) {


//    def &&(p: Prop) = Prop {
//      p.run(0) match {
//        case p =>
//      }
//    }
//
//    def ||(p: Prop): Prop {
//
//    }

  }


  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    def unit[A](a: => A): Gen[A] =
      Gen(State.unit(a))

    def boolean: Gen[Boolean] =
      Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
      Gen(State(RNG.double).flatMap {
        d => if (d < g1Threshold) g1._1.sample else g2._1.sample
      })
    }

  }

  case class Gen[A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => Gen.listOfN(n, this))

  }

}









