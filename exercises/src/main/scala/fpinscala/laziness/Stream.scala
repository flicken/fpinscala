package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = doToList(this, Nil)

  def toListInternally: List[A] = {
    @tailrec
    def doToList[B](s: Stream[B], reversedList: List[B]): List[B] = {
      s match {
        case Cons(head, tail) => doToList(tail(), head() +: reversedList)
        case Empty => reversedList.reverse
      }
    }
    doToList(this, Nil)
  }

  def toListViaFoldRight: List[A] =
    foldRight[List[A]](Nil)((a, b) => a :: b)

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) =>
      if (n == 0) Empty
      else cons(head(), tail().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) =>
      if (n == 0) this else tail().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) =>
      if (p(head())) cons(head(), tail().takeWhile(p)) else Empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) cons(a, b.takeWhileViaFoldRight(p)) else Empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(head, tail) =>
     p(head()) && tail().forAll(p)
  }

  def forAll2ViaFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(head, _) => Some(head())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  @tailrec
  private def doToList[A](s: Stream[A], reversedList: List[A]): List[A] = {
    s match {
      case Cons(head, tail) => doToList(tail(), head() +: reversedList)
      case Empty => reversedList.reverse
    }
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}