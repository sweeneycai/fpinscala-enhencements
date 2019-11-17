package fpinscala.exercise.laziness

import Stream._

sealed trait Stream[+A] {
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @scala.annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def foldLeft[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)
    case _ => z
  }

  // hint : the result is reverse
  def takeWhileViaFoldLeft(f: A => Boolean): Stream[A] =
    foldLeft(empty[A])((h, t) => if(f(h)) cons(h, t) else t)

  // todo
  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t)

  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case Cons(_, t) => t() takeWhile f
    case _ => this
  }

  def exists(p: A => Boolean): Boolean = foldLeft(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Option(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  // 注意flatMap和append的联用
  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

  def find(f: A => Boolean): Option[A] = filter(f).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // 用于创建特定类型Stream的智能构造器，该构造器可以避免重复求值
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    val infiniteStream: Stream[A] = cons(a, infiniteStream)
    infiniteStream
  }

  def constant_2[A](a: A): Stream[A] = cons(a, constant_2(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def helper(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a + b, helper(b, a + b))
    }
    helper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h: A, s: S)) => cons(h, unfold(s)(f))
    case None => Empty
  }

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])
}