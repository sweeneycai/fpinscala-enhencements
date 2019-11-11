package errorhandling

import fpinscala.exercise.errorhandling._
import org.scalatest.FlatSpec

class EitherSpec extends FlatSpec {
  val either = Either(null, 4)
  "map" should "apply a function to Either(a)" in {
    assert(either == Right(4))
    assert(either.map(_ + 1) == Right(5))
  }
}
