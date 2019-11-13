package laziness

import fpinscala.exercise.laziness._
import org.scalatest.FlatSpec

class StreamSpec extends FlatSpec {
  val s = Stream(1, 2, 3)

  "toList" should "return a List of A" in {
    assert(s.toListRecursive == List(1, 2, 3))
    assert(s.toList == List(1, 2, 3))
  }

  "take" should "return first n elements" in {
    assert(s.take(2) == List(1, 2))
  }

  "drop" should "drop first n elements" in {
    assert(s.drop(2) == List(3))
  }

  ""
}
