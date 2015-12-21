package fpinscala.laziness

import fpinscala.BaseSpec


class StreamSpec extends BaseSpec {
  import Stream._
  
  val BiggerThanStack = 1000000

  it("implements toList") {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  it("implements toListViaFoldRight") {
    Stream(1, 2, 3).toListViaFoldRight shouldBe List(1, 2, 3)
  }

  it("implements toList tail recursively") {
    ones.take(BiggerThanStack).toList.size shouldBe BiggerThanStack
  }

  it("implements toListInternally tail recursively") {
    ones.take(BiggerThanStack).toListInternally.size shouldBe BiggerThanStack
  }

  it("does not implements toListViaFoldRight tail recursively") {
    intercept[StackOverflowError] {
      ones.take(BiggerThanStack).toListViaFoldRight.size shouldBe BiggerThanStack
    }
  }

  it("implements take") {
    Stream(1, 2, 3, 4).take(3).toList shouldBe List(1, 2, 3)
  }

  it("implements drop") {
    Stream(1, 2, 3, 4).drop(3).toList shouldBe List(4)
  }

  it("implements takeWhile") {
    Stream(1, 2, 3, 4).takeWhile(_ < 3).toList shouldBe List(1, 2)
  }

  it("implements takeWhileViaFoldRight") {
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ < 3).toList shouldBe List(1, 2)
    Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ > 3).toList shouldBe List()
  }

  it("implements takeWhileViaFoldRight tail recursively") {
    ones.take(BiggerThanStack).takeWhile(_ < 0) shouldBe Empty
    ones.take(BiggerThanStack).takeWhile(_ > 0).toList.size shouldBe BiggerThanStack
  }

  it("implements forAll") {
    Stream(1, 2, 3, 4).forAll(_ < 3) shouldBe false
    Stream(1, 2, 3, 4).forAll(_ > 0) shouldBe true
  }

  it("implements forAll2ViaFoldRight") {
    Stream(1, 2, 3, 4).forAll2ViaFoldRight(_ < 3) shouldBe false
    Stream(1, 2, 3, 4).forAll2ViaFoldRight(_ > 0) shouldBe true
  }

  it("does not implement forAll2ViaFoldRight tail recursively") {
    ones.take(BiggerThanStack).forAll2ViaFoldRight(_ < 0) shouldBe false
    intercept[StackOverflowError] {
      ones.take(BiggerThanStack).forAll2ViaFoldRight(_ > 0) shouldBe true
    }
  }

  it("implements find tail recursive") {
    ones.take(BiggerThanStack).find(_ == 2) shouldBe None
  }

  it("implements map") {
    Stream(1, 2).map(_ + 1).toList shouldBe List(2, 3)
  }

  it("implements filter") {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  it("implements headOptionViaFoldRight") {
    Stream(1, 2).headOptionViaFoldRight shouldBe Some(1)
    Stream.empty.headOptionViaFoldRight shouldBe None
  }

  it("implements append via fold right") {
    (Stream(1, 2) append Stream(3, 4)).toList shouldBe List(1, 2, 3, 4)
  }

  it("implements flatMap") {
    Stream(1, 3).flatMap(x => Stream(x, x + 1)).toList shouldBe List(1, 2, 3, 4)
  }
}
