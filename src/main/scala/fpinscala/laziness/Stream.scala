package fpinscala.laziness

import fpinscala.laziness.Stream._

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

  @scala.annotation.tailrec
  final def foldLeft[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)
    case _ => z
}

  def takeWhileViaFoldLeft(f: A => Boolean): Stream[A] =
    foldLeft(empty[A])((h, t) => if(f(h)) cons(h, t) else t).reverse

  // so we can reverse the whole Stream just using foldLeft
  def reverse: Stream[A] = foldLeft(empty[A])((h, t) => cons(h, t))

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

  // 部分应用函数很好用
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  // 注意flatMap和append的联用
  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

  def find(f: A => Boolean): Option[A] = filter(f).headOption

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
    case (Cons(h, _), n) if n == 1 => Some((h(), (empty[A], n - 1)))
    case _ => None
  }

  // f takes two parameters, not tuple
  def zipWithAll[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A], t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((a, b) => a -> b)

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)((a, b) => (a, b))

  @scala.annotation.tailrec
  final def startWith[B >: A](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startWith(t2())
    case (Cons(_, _), Empty) => true
    case (Cons(_, _), Cons(_, _)) => false
    case _ => false
  }

  def startWith2[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhileViaFoldLeft(_._2.isDefined) forAll {
      case (a, b) => a == b
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s.drop(1)))
  } append Stream(empty)

  def hasSubSequence[B >: A](s: Stream[B]): Boolean =
    tails exists(_ startWith2 s)

  // todo
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons.
      // So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def helper(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a + b, helper(b, a + b))
    }
    helper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => Empty
  }

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(a => Some((a._1 + a._2, (a._2, a._1 + a._2))))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(a => Some((a, a + 1)))
}