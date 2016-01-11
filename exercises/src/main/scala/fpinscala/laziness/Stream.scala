package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def scanRightViaFoldRight[B](z: B)(f: (A, B) => B) = foldRight(Stream(z)){ case (a, intermediateResults @ Cons(h, _)) =>
      Stream.cons(f(a, h()), intermediateResults)
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
      if (n == 0) {
        Empty
      }
      else {
        lazy val t = tail()
        Cons(head, () => t.take(n - 1))
      }
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

  def headOptionViaFoldRight: Option[A] = foldRight[Option[A]](None) { case (element, _) =>
    Some(element)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B]) { case (element, stream) =>
    cons(f(element), stream)
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { case (element, stream) =>
    if (f(element)) cons(element, stream) else stream
  }

  def append[B >: A](rest: => Stream[B]): Stream[B] = foldRight(rest) { case (element, r) =>
    cons(element, r)
  }

  def flatMap[B](f: A => Stream[B]) = foldRight(Stream.empty[B]) { case (element, stream) =>
    f(element) append stream
  }

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
  val onesViaUnfold: Stream[Int] = unfold(())(_ => Some(1 -> ()))

  def constant[A](c: A): Stream[A] = {
    val s = Stream.cons(c, Stream.constant(c))
    s
  }
  def constantViaUnfold[A](c: A): Stream[A] = unfold(c)(_ => Some(c -> c))

  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n + 1))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(c => Some((c, c + 1)))

  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = cons(a, f(b, a + b))

    f(0, 1)
  }
  def fibsViaUnfold: Stream[Int] = unfold(0 -> 1){ case (a, b) =>
    Some(a, b -> (a + b))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def doit(state: S): Stream[A] = f(state) match {
      case Some((a, s)) => cons(a, doit(s))
      case None => empty
    }
    doit(z)
  }
}