package fpinscala.state

import fpinscala.BaseSpec
import fpinscala.state.RNG._

class RNGSpec extends BaseSpec {
  import RNG._
  case class StaticValue(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = (value, this)
  }
  case class IncreasingValue(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = (value, copy(value + 1))
  }

  it("implements nonNegativeInt - exercise 6.1") {
    nonNegativeInt(StaticValue(2))._1 shouldBe 2
    nonNegativeInt(StaticValue(Int.MinValue))._1 should be >= 0
    nonNegativeInt(StaticValue(Int.MaxValue))._1 shouldBe Int.MaxValue
    nonNegativeInt(StaticValue(1))._1 shouldBe 1
    nonNegativeInt(StaticValue(-1))._1 shouldBe 1
  }

  it("implements double -- exercise 6.2") {
    double(StaticValue(0))._1 shouldBe 0
    double(StaticValue(Int.MinValue))._1 shouldBe 0
    double(StaticValue(Int.MaxValue - 1))._1 should be < 1.0
    double(StaticValue(Int.MaxValue))._1 should be < 1.0
  }

  it("implements intDouble -- exercise 6.3") {
    intDouble(IncreasingValue(-1))._1 shouldBe (1, 0.0)
  }

  it("implements doubleInt -- exercise 6.3") {
    doubleInt(IncreasingValue(0))._1 shouldBe (0.0, 1)
  }

  it("implements double3 -- exercise 6.3") {
    double3(IncreasingValue(0))._1 shouldBe (0.0, 1.0 / Int.MaxValue, 2.0 / Int.MaxValue)
  }

  it("implements ints -- exercise 6.4") {
    ints(0)(StaticValue(0))._1 shouldBe List()
    ints(1)(StaticValue(1))._1 shouldBe List(1)
    ints(2)(StaticValue(1))._1 shouldBe List(1, 1)
    ints(2)(IncreasingValue(1))._1 shouldBe List(1, 2)
  }

  it("implements intsRecursive -- exercise 6.4") {
    intsRecursive(0)(StaticValue(0))._1 shouldBe List()
    intsRecursive(1)(StaticValue(1))._1 shouldBe List(1)
    intsRecursive(2)(StaticValue(1))._1 shouldBe List(1, 1)
    intsRecursive(2)(IncreasingValue(1))._1 shouldBe List(1, 2)
    intercept[StackOverflowError] {
      intsRecursive(LargerThanStack)(IncreasingValue(1))._1 should have length 100000
    }
  }

  it("implements intsTailRecursive -- exercise 6.4") {
    intsTailRecursive(0)(StaticValue(0))._1 shouldBe List()
    intsTailRecursive(1)(StaticValue(1))._1 shouldBe List(1)
    intsTailRecursive(2)(StaticValue(1))._1 shouldBe List(1, 1)
    intsTailRecursive(2)(IncreasingValue(1))._1 shouldBe List(1, 2)
    intsTailRecursive(LargerThanStack)(IncreasingValue(1))._1 should have length 100000
  }

  val LargerThanStack = 100000
}
