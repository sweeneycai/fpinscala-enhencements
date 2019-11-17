package laziness

import fpinscala.exercise.laziness._
import org.scalatest.FlatSpec

class StreamSpec extends FlatSpec {
  val s = Stream(1, 2, 3, 4, 5)

  "toList" should "return a List of A" in {
    assert(s.toListRecursive == List(1, 2, 3, 4, 5))
    assert(s.toList == List(1, 2, 3, 4, 5))
  }

  "take" should "return first n elements" in {
    assert(s.take(2).toList == List(1, 2))
  }

  "drop" should "drop first n elements" in {
    assert(s.drop(2).toList == List(3, 4, 5))
  }

  "takeWhile" should "take elements that satisfy f" in {
    assert(s.takeWhileViaFoldRight(_ < 2).toList == List(1))
    assert(s.takeWhileViaFoldRight(_ > 0).toList == s.toList)
    // todo reverse
    assert(s.takeWhileViaFoldLeft(_ > 0).toList.reverse == s.toList)
    assert(s.takeWhileViaFoldLeft(_ < 2).toList == List(1))
    assert(s.takeWhile(_ > 2).toList == List(3, 4, 5))
    assert(s.takeWhileViaFoldLeft(_ > 2).toList.reverse == List(3, 4, 5))
    assert(s.takeWhileViaFoldRight(_ > 2).toList == List(3, 4, 5))
  }

  "exists" should "return false if s doesn't contain the element that satisfy f" in {
    assert(s.exists(_ > 0))
    assert(!s.exists(_ < 0))
    assert(s.forAll(_ > 0))
    assert(!s.forAll(_ > 2))
  }

  "headOption" should "return first element of s" in {
    assert(s.headOption == Option(1))
    assert(Stream().headOption.isEmpty)
  }

  "map" should "apply function f to every element of s" in {
    assert(s.map(_ + 1).takeWhile(_ > 3).toList == List(4, 5, 6))
  }

  "filter" should "works the same as takeWhile" in {
    assert(s.filter(_ > 3).toList == List(4, 5))
    assert(s.filter(_ < 0).toList == List())
  }

  "append" should "append another Stream after this one" in {
    assert(s.append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 4, 5, 6))
  }

  "flatMap" should "apply f to s" in {
    assert(s.flatMap(a => Stream(a + 1)).toList == List(2, 3, 4, 5, 6))
  }

  "find" should "find first element that satisfy f" in {
    assert(s.find(_ == 2).toList == List(2))
  }

  "infinite stream" should "return number of values that we want" in {
//    这个地方虽然编译的时候会报错，但是在repl里面是可以直接运行的
    val ones: Stream[Int] = Stream.cons(1, ones)
    assert(ones.take(3).toList == List(1, 1, 1))
  }
}
