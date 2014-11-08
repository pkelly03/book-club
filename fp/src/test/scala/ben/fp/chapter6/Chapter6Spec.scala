package ben.fp.chapter6


import ben.fp.chapter6.Machine.MachineState
import org.scalatest.{Matchers, WordSpec}


class Chapter6Spec extends WordSpec with Matchers {

  import ben.fp.chapter6.RNG._

  val seed: SimpleRNG = SimpleRNG(1)

  "6.1 RNG nonNegativeInt" should {

    "generate a random integer between 0 and Int.maxValue (inclusive)" in {

      val (int, rng) = nonNegativeInt(SimpleRNG(Int.MinValue))
      int shouldBe 1932951552
    }
  }

  "6.2 RNG" should {

    "create a reproducable double" in {

      double(seed) shouldBe RNG.double(seed)
      double(seed)._1 shouldBe 1.7916224896907806E-4
    }
  }


  "6.3 RNG" should {

    "intDouble" in {

      intDouble(seed) match {
        case ((i: Int, d: Double), r: RNG) =>
        case _ => fail("no ints, doubles or RNGs?")
      }
    }

    "doubleInt" in {

      doubleInt(seed) match {
        case ((d: Double, i: Int), r: RNG) =>
        case _ => fail("no doubles, ints or RNGs?")
      }
    }

    "double3" in {

      double3(seed) match {
        case ((d: Double, d2: Double, d3: Double), r: RNG) =>
        case _ => fail("3 doubles and an RNG!")
      }
    }
  }


  "6.4 RNG" should {

    "generate a list of random integers" in {

      val inty: (List[Int], RNG) = ints(10)(seed)
      inty._1.length shouldBe 10

      val inty2: (List[Int], RNG) = ints(0)(seed)
      inty2._1 shouldBe Nil
    }
  }

  "6.5 RNG" should {

    "create a reproducable double" in {

      double2 {
        SimpleRNG(42)
      } shouldBe(0.007524831689672932, SimpleRNG(1059025964525L))
      double2 {
        SimpleRNG(1059025964525L)
      } shouldBe(0.5967354856416283, SimpleRNG(197491923327988L))
      double2 {
        SimpleRNG(197491923327988L)
      } shouldBe(0.15846728447753344, SimpleRNG(259172689157871L))
      double2 {
        SimpleRNG(259172689157871L)
      } shouldBe(0.9386595436086224, SimpleRNG(149370390209998L))
    }
  }

  "6.6 RNG" should {

    "create a reproducable double" in {

      map2(nonNegativeInt, nonNegativeInt)(_ + _)(SimpleRNG(42)) shouldBe(1297639150, SimpleRNG(197491923327988L))
    }
  }

  "6.7 RNG" should {

    "sequence a list of transition" in {

      sequence(List()) {
        SimpleRNG(42)
      } shouldBe(Nil, SimpleRNG(42))
      sequence(List(double _)) {
        SimpleRNG(42)
      } shouldBe(List(0.007524831686168909), SimpleRNG(1059025964525L))
    }
  }

  "6.8 RNG, 6.9 reimplement map/flatmap" should {

    "flatmap" in {

      flatMap(nonNegativeInt) { _ => nonNegativeInt} {
        SimpleRNG(42)
      } shouldBe(1281479697, SimpleRNG(197491923327988L))
      flatMap(nonNegativeInt) { a => rng => {
        val (b, rng2) = rng.nextInt
        (b + a, rng2)
      }
      } {
        SimpleRNG(42)
      } shouldBe(-1265320244, SimpleRNG(197491923327988L))
    }

    "implement map in terms of flatMap" in {
      mapv2(nonNegativeInt) {
        _ * 2
      } {
        SimpleRNG(42)
      } shouldBe(32318906, SimpleRNG(1059025964525L))
    }

    "implement map2 in terms of flatMap" in {
      map2v2(nonNegativeInt, nonNegativeInt) {
        (_, _)
      } {
        SimpleRNG(42)
      } shouldBe((16159453, 1281479697), SimpleRNG(197491923327988L))
    }
  }

  val initialString = "foo"
  val initialState = State.unit[String, String](initialString)

  "6.10 State" should {

    "unit" in {

      val (newState, originalState) = initialState.run("baz")
      originalState shouldBe "baz"
      newState shouldBe initialString

      val reversing = State[String, String](a => (a.reverse, a))

      val (newState2, originalState2) = reversing.run(initialString)
      originalState2 shouldBe initialString
      newState2 shouldBe initialString.reverse
    }

    "map" in {


      val reversedState = initialState.map(s => s.reverse)
      val (newString, originalString) = reversedState.run("jinks")

      newString shouldBe initialString.reverse
      originalString shouldBe "jinks"
    }

    "map2" in {

      val reverse = State[String, String](s => (s.reverse, s))
      val caps = State[String, String](s => (s.toUpperCase, s))
      val (result, input) = caps.map2(reverse)((a, b) => s"$a/$b").run(initialString)

      result shouldBe "FOO/oof"
      input shouldBe initialString
    }

    "flatMap" in {
      val reverse = State[String, String](s => (s.reverse, s))
      val caps = State[String, String](s => (s.toUpperCase, s))

      val (result, input) = reverse.flatMap( p => caps ).run("hello there")
      result shouldBe "HELLO THERE"
      input shouldBe "hello there"
    }

    "sequence" in {
      val reverse = State[String, String](s => (s.reverse, s))
      val caps = State[String, String](s => (s.toUpperCase, s))

      val states = State.sequence(List(initialState, reverse, caps))
      val (result,input) = states.run("abcdef")

      result.size shouldBe 3
      input shouldBe "abcdef"

      result(0) shouldBe initialString
      result(1) shouldBe input.reverse
      result(2) shouldBe input.toUpperCase
    }
  }


  "6.11 Coin state machine"   should {

    def simulator = MachineSimulation(coins = 0, sweets = 10)

    "Initial state has 10 sweets and no coins" in {

      val machine: MachineState = simulator.simulateMachine(Nil)

      val ((coins, sweets), nextState) = machine.run(Machine(locked = true, 10,0))

      coins shouldBe 0

      sweets shouldBe 0

      nextState shouldBe Machine(locked = true, 10,0)
    }

    "Inserting a coin into a locked machine will cause it to unlock if there are any sweets left" in {

      val machine: MachineState = simulator.simulateMachine(List(Coin))

      val ((sweets, coins), nextState) = machine.run(Machine(locked = true, 10,0))

      sweets shouldBe 10

      coins shouldBe 1

      nextState.locked shouldBe false
    }

    "Turning the knob on an unlocked machine will cause it to dispense candy and become lockeds left" in {

      val machine: MachineState = simulator.simulateMachine(List(Coin, Turn))

      val ((sweets, coins), nextState) = machine.run(Machine(locked = true, 10,0))

      sweets shouldBe 9

      coins shouldBe 1

      nextState.locked shouldBe true
    }
  }
}
