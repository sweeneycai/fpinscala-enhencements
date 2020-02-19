package fpinscala.errorhandling

import fpinscala.BaseSpec

class EitherSpec extends BaseSpec {
  val either: Either[Null, Int] = Either(null, 4)
  "map" should "apply a function to Either(a)" in {
    assert(either == Right(4))
    assert(either.map(_ + 1) == Right(5))
  }
}
