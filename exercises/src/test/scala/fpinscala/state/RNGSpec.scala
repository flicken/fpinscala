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
    println(double(StaticValue(Int.MaxValue))._1)
    println(double(StaticValue(Int.MaxValue - 1))._1)
    double(StaticValue(0))._1 shouldBe 0
    double(StaticValue(Int.MinValue))._1 shouldBe 0
    double(StaticValue(Int.MaxValue - 1))._1 should be < 1.0
    double(StaticValue(Int.MaxValue))._1 should be < 1.0
    double(StaticValue(Int.MaxValue - 1))._1 should not be double(StaticValue(Int.MaxValue))._1
  }

  it("implements intDouble -- exercise 6.3") {
    intDouble(IncreasingValue(-1))._1 shouldBe (-1, 0.0)
  }

  it("implements doubleInt -- exercise 6.3") {
    doubleInt(IncreasingValue(0))._1 shouldBe (0.0, 1)
  }

  it("implements double3 -- exercise 6.3") {
    double3(IncreasingValue(0))._1 shouldBe (0.0, 1.0 / (Int.MaxValue.toDouble + 1), 2.0 / (Int.MaxValue.toDouble + 1))
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


  it("implements doubleViaMapViaMap -- exercise 6.5") {
    println(doubleViaMap(StaticValue(Int.MaxValue))._1)
    println(doubleViaMap(StaticValue(Int.MaxValue - 1))._1)
    doubleViaMap(StaticValue(0))._1 shouldBe 0
    doubleViaMap(StaticValue(Int.MinValue))._1 shouldBe 0
    doubleViaMap(StaticValue(Int.MaxValue - 1))._1 should be < 1.0
    doubleViaMap(StaticValue(Int.MaxValue))._1 should be < 1.0
    doubleViaMap(StaticValue(Int.MaxValue - 1))._1 should not be doubleViaMap(StaticValue(Int.MaxValue))._1
  }

  it("implements map2 -- exercise 6.6") {
    RNG.map2(nonNegativeInt, nonNegativeInt)((_, _))(IncreasingValue(12))._1 shouldBe (12, 13)
    RNG.map2(nonNegativeInt, nonNegativeInt)(List(_, _))(IncreasingValue(12))._1 shouldBe List(12, 13)
  }

  it("implements both -- exercise 6.6") {
    RNG.intDoubleViaBoth(IncreasingValue(-1))._1 shouldBe(-1, 0.0)
    RNG.doubleIntViaBoth(IncreasingValue(0))._1 shouldBe(0.0, 1)
  }

  it("implements sequence -- exercise 6.7") {
    import RNG._
    sequence(List(int, int, double _))(IncreasingValue(-2))._1 shouldBe List(-2, -1, 0.0)
  }

  it("implements intsViaSequences -- exercise 6.7") {
    RNG.intsViaSequence(10)(IncreasingValue(-5))._1 shouldBe List(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4)
  }

  it("implements flatMap -- exercise 6.8") {
    val doubleIfZero = flatMap(int)(i => if (i == 0) double3 else int)

    doubleIfZero(StaticValue(0))._1 shouldBe ((0.0, 0.0, 0.0))
    doubleIfZero(StaticValue(1))._1 shouldBe 1
  }

  val LargerThanStack = 100000
}
