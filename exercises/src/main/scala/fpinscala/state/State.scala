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
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
