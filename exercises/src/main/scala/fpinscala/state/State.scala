package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s){ a => (f(a), _)}

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    flatMap(rb) { b =>
      (f(a, b), _)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    if (i < n) (i, _) else nonNegativeLessThan(n)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    val value = if (i == Int.MinValue) 0 else Math.abs(i)
    (value, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    val value = i / (Int.MaxValue.toDouble + 1)
    (value, nextRng)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  val intDoubleViaBoth: Rand[(Int, Double)] = both(int, double)
  val doubleIntViaBoth: Rand[(Double, Int)] = both(double, int)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var currRng = rng // Oh noes!  A var?!?

    val values = List.fill(count) {
      val (next, nextRng) = nonNegativeInt(currRng)
      currRng = nextRng
      next
    }
    (values, currRng)
  }

  def intsRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (i, nextRng) = nonNegativeInt(rng)
      val (list, lastRng) = intsRecursive(count - 1)(nextRng)
      (i :: list, lastRng)
    }
  }

  def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def doIt(count: Int, is: Vector[Int], r: RNG): (List[Int], RNG) = {
      if (count == 0) {
        (is.toList, r)
      } else {
        val (i, nextR) = nonNegativeInt(r)
        doIt(count - 1, is :+ i, nextR)
      }
    }
    doIt(count, Vector(), rng)
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rnga) = ra(rng)
    val (b, rngb) = rb(rnga)

    (f(a, b), rngb)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    var currRng = rng
    val vs = fs.map { f =>
      val (v, nextRng) = f(currRng)
      currRng = nextRng
      v
    }
    (vs, currRng)
  }


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (value, rng1) = f(rng)
    g(value)(rng1)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State({ s =>
      val (a, s1) = run(s)
      (f(a), s1)
    })
  def mapViaFlatMap[B](f: A => B): State[S, B] = flatMap(a => State((f(a), _)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap { a =>
    sb.flatMap { b =>
      State((f(a, b), _))
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State({ s =>
    val (a: A, s1: S) = run(s)
    f(a).run(s1)
  })

  // def simulate(inputs: List[A])(f: (S, A) => S)
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State({ m =>
    val finalMachine = inputs.foldLeft(m)(nextState)
    ((finalMachine.coins, finalMachine.candies), finalMachine)
  })

  def nextState(m: Machine, i: Input) = (m, i) match {
    case (m@Machine(true, candies, _), Coin) if candies > 0 =>
      val newCoins = m.coins + 1
      m.copy(locked = false, coins = newCoins)
    case (m@Machine(false, candies, _), Turn) if candies > 0 =>
      val newCandy = m.candies - 1
      m.copy(locked = true, candies = newCandy)
    case (m, _) => m
  }

  def simulateMachineViaSequence(inputs: List[Input]): State[Machine, (Int, Int)] = sequence(inputs.map { input =>
      State[Machine, Machine]({m =>
        val next = nextState(m, input)
        (next, next)
      })
    }).map(_.last).map(m => (m.coins, m.candies))

  def simulateMachineViaFlatMap(inputs: List[Input]): State[Machine, List[Input]] = {
    null
  }

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State[S, List[A]]({ rng =>
    var currRng = rng
    val vs = fs.map { f =>
      val (v, nextRng) = f.run(currRng)
      currRng = nextRng
      v
    }
    (vs, currRng)
  })

}
