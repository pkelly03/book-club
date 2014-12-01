package ben.fp.chapter7

import java.util.concurrent._

import ben.fp.chapter7.Par._
import org.scalatest.{Matchers, WordSpec}

class Chapter7Spec extends WordSpec with Matchers {

  def es = new ForkJoinPool()
  def timeout(l:Long) = l


  val parA: Par[Int] = unit(2)
  val parB: Par[Int] = unit(1)


  "Par 7.2" should {

    "have the function map2" in {

      map2(parB, parA)(_ + _)(es) shouldBe UnitFuture(3)
    }
  }


  "Par 7.3 - map2 with timeout" should {

    def future[A](a:A, sleep:Long = 0) :Future[A]=  es.submit( new Callable[A] {
      override def call(): A = {
        Thread.sleep(sleep)
        a
      }})

    "complete if done within timeout" in {

      val par1 = timedUnit(future[Int](2, 100))
      val par2 = timedUnit(future[Int](1, 100))

      val adder = map2WithTimeout(par1, par2,  timeout = 300L, TimeUnit.MILLISECONDS)(_ + _)
      adder(es) shouldBe UnitFuture(3)
    }

    "throw exception if not completed within timeout" in {

      val par1 = timedUnit(future[Int](2,300))
      val par2 = timedUnit(future[Int](1,400))

      intercept[TimeoutException] {
        val adder = map2WithTimeout(par1, par2, timeout = 310L, TimeUnit.MILLISECONDS)(_ + _)
        adder(es)
      }
    }
  }

  "Par 7.4" should {

    "asyncF" in {
      val function = asyncF[String, Int] (string => Integer.parseInt(string))
      val parOfStringToInt:Par[Int] = function("1")
      val future: Future[Int] = parOfStringToInt(es)
    }
  }

  case class Clock(start:Long) {
    var s = start
    def now:Long =  {
      val r = s
      s = s + 1
      r
    }
  }

  "Par 7.5" should {

    "sequencce" in {

      val clock: Clock = Clock(0L)
      val ordered = sequence( List.fill(5)(unit(clock.now)) )(es)
      val result = ordered.get(10L, TimeUnit.MILLISECONDS)

      val clock2: Clock = Clock(0L)
      result shouldBe List.fill(5)(unit(clock2.now)(es).get)
    }

    "sequence simple" in {

      val clock: Clock = Clock(0L)
      val ordered = sequence_1st_attempt( List.fill(5)(unit(clock.now)) )(es)
      val result = ordered.get(10L, TimeUnit.MILLISECONDS)

      val clock2: Clock = Clock(0L)
      result shouldBe List.fill(5)(unit(clock2.now)(es).get)
    }
  }

  "deadlock!! " should {

    "innit" in {
      val a = lazyUnit(42 + 1)
      val S = Executors.newFixedThreadPool(2)
      println(Par.equal(S)(a, fork(a)))

    }
  }
}










































