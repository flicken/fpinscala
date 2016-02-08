package fpinscala.state

import fpinscala.BaseSpec

class StateSpec extends BaseSpec {
  import State._
  type MachineState[A] = State[Machine, A]

  it("implements unit") {

    unit("a").run(Machine(true, 1, 0))._1 shouldBe "a"
  }

  it("implements flatMap") {
    unit[Machine, String]("a").flatMap(a => State.unit(a + "b")).run(Machine(true, 1, 0))._1 shouldBe "ab"
  }

  it("implements map") {
    unit[Machine, String]("a").map(_ + "b").run(Machine(true, 1, 0))._1 shouldBe "ab"
  }

  it("implements map2") {
    val a = State.unit[Machine, String]("a")
    val b = State.unit[Machine, String]("b")
    unit[Machine, String]("a").map2(b)(_ + _).run(Machine(true, 1, 0))._1 shouldBe "ab"
  }

  it("implements sequence") {
    val a = State.unit[Machine, String]("a")
    val b = State.unit[Machine, String]("b")
    sequence(List(a, b)).run(Machine(true, 1, 0))._1 shouldBe List("a", "b")
  }

  describe("machine") {
    it("unlocks machine if unlocked and candy left") {
      val m = Machine(true, 1, 0)
      simulateMachine(List(Coin)).run(m)._2.locked shouldBe false
    }
    it("turning knob on unlocked machine causes it to dispense candy and become locked") {
      val m = Machine(false, 1, 1)
      simulateMachine(List(Turn)).run(m)._2 shouldBe Machine(true, 0, 1)
    }
    it("turning the knob on a locked machine does nothing") {
      val m = Machine(true, 1, 1)
      simulateMachine(List(Turn)).run(m)._2 shouldBe m
    }

    it("inserting a coin into an unlocked machine does nothing") {
      val m = Machine(false, 1, 1)
      simulateMachine(List(Coin)).run(m)._2 shouldBe m
    }

    it("machine that's out of candy ignores all inputs") {
      val m = Machine(false, 0, 99)
      simulateMachine(List(Turn, Coin, Coin, Turn)).run(m)._2 shouldBe m
    }

    it("successfully processes example") {
      val m = Machine(true, 5, 10)
      val buy = List(Coin, Turn)
      simulateMachine(buy ++ buy ++ buy ++ buy).run(m) shouldBe ((14, 1), Machine(true, 1, 14))
    }
  }

}
