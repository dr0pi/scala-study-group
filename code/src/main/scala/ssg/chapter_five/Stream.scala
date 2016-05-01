package ssg.chapter_five

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
  /*
  def take(n: Int): Stream[A] = {
    @tailrec
    def go(n: Int, org: Stream[A], acc: Stream[A]): Stream[A] = org match {
      case Cons(h, t) if n > 0 => go(n - 1, t(), cons(h(), acc))
      case _ => acc
    }

    go(n, this, Empty)
  }*/

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(n : Int, org: Stream[A]): Stream[A] = org match {
      case Cons(h, t) if n > 0 => go(n-1, t())
      case _ => org
    }

    go(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) =>
    if (p(h)) {
      cons(h, t)
    }
     else {
      empty
    }
  )

//  def forAll(p: A => Boolean): Boolean = this match {
//    case Cons(h, empty) => (p(h()))
//    case Cons(h, t) if p(h()) => t().forAll(p)
//    case _ => false
//  }

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(head, tail) => Some(head())
  }

  def headOption1: Option[A] = foldRight(None: Option[A])((h: A, _) => Some(h))

  def toList: List[A] = {
    @tailrec
    def go(c: Stream[A], acc: List[A]): List[A] = c match {
      case Cons(h, t) => go(t(), acc ::: List(h()))
      case Empty => acc
    }

    go(this, Nil)
  }
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, acc) => cons(f(h), acc))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, acc) => {
    if (f(h)) {
      cons(h, acc)
    } else {
      acc
    }
  })

  def append[B>:A](that: => Stream[B]): Stream[B] = {
    foldRight(that)((h: A, acc) => cons(h, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, acc) => f(h) append acc)

  //5.13
  def mapu[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeWhileu(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](that: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, that)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(),h2()), (t1(), t2()))
    case _ => None
  }

  def zip[B](that: Stream[B]): Stream[(A,B)] = zipWith(that)((a,b) => (a,b))
  def zipAll[B](that: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, that)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()),Some(h2())), (t1(), t2()))
    case (       empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case (Cons(h1, t1), empty       ) => Some((Some(h1()), None), (t1(), empty))
    case _                            => Some((None, None), (empty, empty))
  }

//  def zipWithAll[B,C](that: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = unfold((this, that)) {
//    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()),Some(h2())), (t1(), t2()))
//    case (empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (empty[A], t2()))
//    case (Cons(h1, t1), empty) => Some(f(Some(h1()), None), (t1(), empty[B]))
//    case _ => Some(f(None, None), (empty[A], empty[B]))
//  }

  //5.14
  def startsWith[B](that: Stream[B]): Boolean = zipAll(that)
    .takeWhile{
      case (_, None)  => false
      case (None, _)  => false
      case _          => true
    }
    .forAll{
      case (Some(a), Some(b)) => a == b
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
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
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fib(): Stream[Int] = {
    def go(last: Int, curr: Int): Stream[Int] = cons(last, go(curr, curr+last))
    go(0, 1)
  }
  def constant[A](a: A): Stream[A] = Cons(() => a, () => constant(a))
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }

  def fibu(): Stream[Int] = unfold((0,1))(
    (z: (Int, Int)) => z match {
      case (prev: Int, curr: Int) => Some(prev, (curr, prev+curr))
    }
  )

  def fromu(n: Int): Stream[Int] = unfold(n)(
    (z: Int) => z match {
      case (curr: Int) => Some(curr, curr+1)
    }
  )

  def constantu[A](a: A): Stream[A] = unfold(a)({
    case (a: A) => Some(a, a)
  })

  def onesu(): Stream[Int] = unfold(1){ n: Int => Some(n, n) }
}