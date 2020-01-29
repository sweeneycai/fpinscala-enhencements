package fpinscala.datastructure

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

object Tree {

  implicit class TreeMethods[A](tree: Tree[A]) {
    def depth: Int = {
      def goDepth(tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => (goDepth(l) max goDepth(r)) + 1
      }
      goDepth(tree)
    }

    def size: Int = {
      def goSize(tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => goSize(l) + goSize(r) + 1
      }
      goSize(tree)
    }

    def map[B](f: A => B): Tree[B] = {
      def goMap(tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r)  => Branch(goMap(l)(f), goMap(r)(f))
      }
      goMap(tree)(f)
    }

    def max(f: (A, A) => Boolean): A = {
      def goMax(tree: Tree[A])(f: (A, A) => Boolean): A = tree match {
        case Leaf(a) => a
        case Branch(l, r) =>
          if (f(goMax(l)(f), goMax(r)(f))) goMax(l)(f)
          else goMax(r)(f)
      }
      goMax(tree)(f)
    }

    def fold[B](f: A => B)(g: (B, B) => B): B = {
      def goFold(tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
        case Leaf(a) => f(a)
        case Branch(l, r) => g(goFold(l)(f)(g), goFold(r)(f)(g))
      }
      goFold(tree)(f)(g)
    }
  }
}

