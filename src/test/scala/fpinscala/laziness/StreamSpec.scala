package fpinscala.laziness

import fpinscala.BaseSpec
import fpinscala.exercise.laziness._

class StreamSpec extends BaseSpec {

  val s: Stream[Int] = Stream(1, 2, 3, 4, 5)

  "toList" should "return a List of A" in {
    assert(s.toListRecursive == List(1, 2, 3, 4, 5))
    assert(s.toList == List(1, 2, 3, 4, 5))
  }

  "take" should "return first n elements" in {
    assert(s.take(2).toList == List(1, 2))
    assert(s.takeViaUnfold(3).toList == List(1, 2, 3))
  }

  "drop" should "drop first n elements" in {
    assert(s.drop(2).toList == List(3, 4, 5))
  }

  "takeWhile" should "take elements that satisfy f" in {
    assert(s.takeWhileViaFoldRight(_ < 2).toList == List(1))
    assert(s.takeWhileViaFoldRight(_ > 0).toList == s.toList)
    // todo reverse
    assert(s.takeWhileViaFoldLeft(_ > 0).toList == s.toList)
    assert(s.takeWhileViaFoldLeft(_ < 2).toList == List(1))
    assert(s.takeWhile(_ > 2).toList == List(3, 4, 5))
    assert(s.takeWhileViaFoldLeft(_ > 2).toList == List(3, 4, 5))
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
    assert(s.mapViaUnfold(_ + 1).takeWhile(_ > 3).toList == List(4, 5, 6))
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

  "fibs" should "return specific nums" in {
    assert(Stream.fibsViaUnfold.take(5).toList == List(1, 2, 3, 5, 8))
  }

  "from" should "return specific nums" in {
    assert(Stream.from(5).take(3).toList == List(5, 6, 7))
  }

  "constant" should "return a Stream of this constant" in {
    //    这个地方虽然编译的时候会报错，但是在repl里面是可以直接运行的
    //    val ones: Stream[Int] = Stream.cons(1, ones)
    //    assert(ones.take(3).toList == List(1, 1, 1))
    assert(Stream.constant(1).take(3).toList == List(1, 1, 1))
    assert(Stream.constantViaUnfold(1).take(3).toList == List(1, 1, 1))
  }

  "reverse" should "reverse the whole Stream" in {
    assert(s.reverse.toList == s.toList.reverse)
  }

  "zipAll" should "zip two Stream together" in {
    println(s.zipAll(Stream(Stream.empty[Int])).toList)
    println(s.zipAll(s).toList)
    println(s.zipAll(s.take(1)).toList)
  }

  "startWith" should "return true if Stream a start with Stream b " in {
    assert(s.startWith(s.take(3)))
    assert(s.startWith(Stream.empty))
    assert(!s.startWith(Stream(3, 2, 1)))
    assert(!s.startWith(Stream("a")))

    assert(s.startWith2(s.take(3)))
    assert(s.startWith2(Stream.empty))
    assert(!s.startWith2(Stream(3, 2, 1)))
    assert(!s.startWith2(Stream("a")))
  }

  "hasSubSequence" should "return true when a contains b" in {
    assert(s.hasSubSequence(s.take(3)))
    assert(s.hasSubSequence(s.drop(3)))
    assert(!s.hasSubSequence(s.drop(3).reverse))
  }
}
