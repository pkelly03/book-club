package ben.fp.chapter6

import ben.fp.chapter6.Machine.{Coins, Sweets}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)


  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, gen) if i == Int.MinValue => (0, gen)
      case (i, gen) => (Math.abs(i), gen)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, gen) = rng.nextInt
    val (dub, gen2) = double(rng)
    (int -> dub) -> gen2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), g) = intDouble(rng)
    (d -> i) -> g
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dub1, gen1) = double(rng)
    val (dub2, gen2) = double(gen1)
    val (dub3, gen3) = double(gen2)
    (dub1, dub2, dub3) -> gen3
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft(List.empty[Int] -> rng) {
      case ((intList, rg), i) =>
        val (int, gen) = nonNegativeInt(rg)
        (int +: intList, gen)
    }
  }

  def unit[A](a: A): Rand[A] = (rng: RNG) => (a, rng)

  def double2(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_ / Int.MaxValue.toDouble)(rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))


  def mapv2[A, B](randA: Rand[A])(f: A => B): Rand[B] = flatMap(randA)(a => rng => (f(a), rng))

  def map2v2[A, B, C](randA: Rand[A], randB: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(randA) {
    a => flatMap(randB) {
      b => rng => (f(a, b), rng)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
}

object State {

  def unit[S, A](a: A): State[S, A] = State(state => (a, state))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    ra.flatMap(a => rb.map(f(a, _)))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = {
    ss.foldRight(State.unit[S, List[A]](List.empty[A])) {
      (state, states) => map2(state, states)(_ :: _)
    }
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] =
    State.map2(this, s)(f)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, b) = run(s)
      f(a).run(b)
    }
  )
}


object Machine {

  type Coins = Int
  type Sweets = Int
  type MachineState = State[Machine, (Coins, Sweets)]

  def input(m: MachineState, i: Input): MachineState = i match {
    case Coin => m.flatMap{
      case (coins, sweets) => State( machine =>
        if (machine.sweets > 0 && machine.locked) (coins +1, sweets) -> machine.copy(locked=false)
        else (coins, sweets) -> machine
      )
    }
    case Turn => m.flatMap{
      case (coins, sweets) => State( machine =>
        if (machine.sweets > 0 && !machine.locked) (coins, sweets - 1) -> machine.copy(locked = true)
        else (coins, sweets) -> machine

      )
    }
  }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, sweets: Int, coins: Int)

case class MachineSimulation(coins:Int, sweets:Int) {

  def simulateMachine(inputs: Input*): ((Coins, Sweets), Machine) = {

    val initialState = State.unit[Machine, (Coins, Sweets)]((coins, sweets))
    val start: Machine = Machine(true, sweets, coins)

    inputs.toList match {
      case Nil => initialState.run(start)
      case _  => inputs.reverse.foldRight(initialState)((input, state) => Machine.input(state, input)).run(start)
    }
  }
}

