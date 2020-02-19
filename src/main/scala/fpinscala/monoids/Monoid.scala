package fpinscala.monoids

import fpinscala.parallelism.Nonblocking.Par._
import fpinscala.parallelism.Nonblocking._

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: Nil.type = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g

    def zero: A => A = (a: A) => a
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  //  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???
  //
  //  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a1, a2)

    def zero: A = m.zero
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  val intCompare: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
    def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
      (a1, a2) match {
        case (Some((x1, y1, p)), Some((x2, y2, q))) =>
          // 并行记录一个有序列表
          Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }

    def zero: Option[(Int, Int, Boolean)] = None
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
  //    foldMapV(ints, intCompare)(i => Some((i, i, true))).map(_._3).getOrElse(true)
    foldMapV(ints, intCompare)(i => Some((i, i, true))).forall(_._3)

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)

    def zero: Par[A] = Par.unit(m.zero)
  }

  // This implementation is quit difficult
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }

    def zero: WC = Stub("")
  }

  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def unstub(s: String): Int = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A],
                          B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    def zero: (A, B) = (A.zero, B.zero)
  }

  def coproductMonoid[A, B](A: Monoid[A],
                            B: Monoid[B]): Monoid[Either[A, B]] = new Monoid[Either[A, B]] {
    def op(a1: Either[A, B], a2: Either[A, B]): Either[A, B] = (a1, a2) match {
      case (Left(_a1), Left(_a2)) => Left(A.op(_a1, _a2))
      case (Right(b1), Right(b2)) => Right(B.op(b1, b2))
      case _ => Left(A.zero)
    }

    def zero: Either[A, B] = Left(A.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))

    def zero: A => B = _ => B.zero
  }

  /**
   * As long as V is a monoid, we can merge key-value Maps using this combinator. Here is a
   * use example
   *
   * @example {{{
   *   val M: Monoid[Map[String, Map[String, Int]]] =
   *     mapMergeMonoid(mapMergeMonoid(intAddition))
   *   val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
   *   val m2 = Map("o1" -> Map("i1" -> 2))
   *   val m3: Map[String, Map[String, Int]] = M.op(m1, m2)
   * }}}
   * @param V Monoid of return type `V`
   * @tparam K Key type
   * @tparam V Return type
   * @return A new Map monoid
   */
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }

    def zero: Map[K, V] = Map[K, V]()
  }

  // Example:
  val M: Monoid[Map[String, Map[String, Int]]] =
    mapMergeMonoid(mapMergeMonoid(intAddition))
  val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
  val m2 = Map("o1" -> Map("i1" -> 2))
  val m3: Map[String, Map[String, Int]] = M.op(m1, m2)

  val M2: Monoid[Map[String, Int]] = mapMergeMonoid(intAddition)

  // This function equals bag, and this function is for older Edition of this book.
  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] =
    strings.foldLeft(List[String]())((acc, s) => acc ++ s.split(" ").toList)
      .map(s => Map(s -> 1)).foldLeft(M2.zero)(M2.op)

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldRight(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))

  override def toList[A](as: IndexedSeq[A]): List[A] = as.toList
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(value) => f(value, z)
    case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(value) => f(value)
    case None => mb.zero
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(value) => f(z, value)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(value) => f(value, z)
  }
}


