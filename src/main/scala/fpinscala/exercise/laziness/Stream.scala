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

  def take(n: Int): List[A] = this match {
    case Cons(h, t) if n > 0 => h() :: t().take(n - 1)
    case _ => Nil
  }

  def drop(n: Int): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) if n <= 0 => h() :: t().drop(n - 1)
    case Cons(_, t) => t().drop(n - 1)
  }

  @scala.annotation.tailrec
  final def drop2(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop2(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => Empty
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // 用于创建特定类型Stream的智能构造器，该构造器可以避免重复求值
  def cons[A](hd: => A, tl: Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}