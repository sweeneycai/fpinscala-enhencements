package fpinscala.datastructure

import fpinscala.BaseSpec

class TreeSpec extends BaseSpec {
  val t: Branch[Int] = Branch(
    Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(3)
    ),
    Branch(Leaf(4), Leaf(5))
  )

  "depth" should "return depth of a Tree" in {
    assert(t.depth == 4)
  }

  "max" should "return max A of Tree[A]" in {
    assert(t.max(_ > _) == 5)
  }

  "map" should "apply f(a) on every element of Tree[A]" in {
    assert(Branch(Leaf(1), Leaf(2)).map(_ + 1) == Branch(Leaf(2), Leaf(3)))
  }

  "size" should "return size of Tree[A]" in {
    assert(t.size == 9)
  }
}
