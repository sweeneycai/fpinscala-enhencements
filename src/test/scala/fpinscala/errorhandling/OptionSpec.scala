package fpinscala.errorhandling

import fpinscala.BaseSpec

class OptionSpec extends BaseSpec {

  val p: Option[Int] = Option(3)

  "map" should "apply function f to Option[A]" in {
    assert(p.map(_ + 1).getOrElse() == 4)
  }

  "filter" should "return Some(a) if Option(a) satisfy f" in {
    assert(p.filter(_ > 2) == Some(3))
  }

  "orElse" should "return ob if Option(a) is None" in {
    assert(p.filter(_ < 2).orElse(Some(4)) == Some(4))
  }

  "getOrElse" should "return a when Option(a) is not null" in {
    assert(p.getOrElse() == 3)
    assert(p.filter(_ < 2).getOrElse() == ())
  }

  "variance" should "return variance of a list when is not null" in {
    assert(Option.variance(List(1, 2, 3)) != null)
    assert(Option.variance(List(0)).getOrElse() == 0)
  }

  "flatMap" should "apply f to Option[A] and return Option[B]" in {
    assert(p.flatMap(a => Option(a.toString)) == Option("3"))
  }

  "sequence" should "return Option[List[A]] when input List[Option[A]]" in {
    assert(Option.sequence(List(Option(1), Option(2), Option(3))) != None)
    assert(Option.sequence(List(Option(1), Option(2), Option(3))) == Option(List(1, 2, 3)))
  }

  "traverse" should "return Option[List[A]] when input List[Option[A]]" in {
    assert(Option.sequence(List(Option(1), Option(2), Option(3))) == Option(List(1, 2, 3)))
  }
}
