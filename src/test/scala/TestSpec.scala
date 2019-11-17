import org.scalatest.FlatSpec

class TestSpec extends FlatSpec {
  "Stream" should "return a Stream of a" in {
    val ones: Stream[Int] = Stream.cons(1, ones)
    ones.take(10).toList
  }
}
