package fpinscala.exercise.datastructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = head.toString + "  " + tail.toString
}

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }


  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(head, tail) => head * product(tail)
  }

  // foldRight 上面两个函数的抽象
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  /**
   *  基于foldRight对上面的sum和product进行重写
   */
  def sum2(ints: List[Int]): Int = foldRight(ints, 0)((x, y) => x + y)

  def product2(ds: List[Double]): Double = // foldRight(ds, 1.0)((x, y) => x * y)
    foldRight(ds, 1.0)(_ * _)

  // 3.2
  def dropFirst[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  // 3.3
  def setFirst[A](h: A, as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => Cons(h, tail)
  }

  // 3.4
  def drop[A](n: Int, as: List[A]): List[A] = {
    @annotation.tailrec
    def go(n: Int, as: List[A]): List[A] = {
      val a  = as match {
        case Nil => Nil
        case Cons(head, tail) => tail
      }
      if (n == 0) as
      else go(n - 1, a)
    }
    go(n, as)
  }

  // 3.5
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    // if 在这里是个卫语句
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init list is empty")
    // 只有处理到最后一个元素时才会进行进一步的处理
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  // 3.9 spark 里面的一个函数
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // 3.11
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // 3.12 reverse
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  // 3.13
  // Normal implementations to avoid stack over flow
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  // 3.14
  def append[A](l: List[A], b: List[A]): List[A] = foldRight(l, b)(Cons(_, _))
}
