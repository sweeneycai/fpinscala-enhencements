package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par.Par
import fpinscala.parallelism._
import fpinscala.state._
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._

import scala.language.{implicitConversions, postfixOps}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) =>
      run(m, n, rng) match {
        case Passed | Proved => p.run(m, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, rng) =>
      run(m, n, rng) match {
        // if this one failed, we then tag p and run p
        case Falsified(msg, _) => p.tag(msg).run(m, n, rng)
        case x => x
      }
  }

  def tag(msg: String): Prop = Prop {
    (m, n, rng) =>
      run(m, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  def apply(run: (TestCases, RNG) => Result): Prop = Prop { (_, n, rng) => run(n, rng) }

  /**
   * This version of run will return Passed, that means, when we testing a property,
   * and runs 100 tests, the result will contain all of the Passed, not Proved this
   * property. So below is another `run`.
   *
   * @param prop      prop
   * @param maxSize   max size
   * @param testCases test cases
   * @param rng       generator
   */
  def run(prop: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    prop.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n $msg")
      case Passed =>
        println(s"+ Ok, passed $testCases tests.")
    }

  def run2(prop: Prop,
           maxSize: Int,
           testCases: Int,
           rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    prop.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ Ok, passed $testCases tests.")
      case Proved =>
        println(s"+ Ok, proved property.")
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase,
                       success: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take(n.min(max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = {
    lazy val result = p
    forAll(unit(()))(_ => result)
  }

  def check2(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  /**
   * Now we can test Par
   */
  val ES: ExecutorService = Executors.newCachedThreadPool
  val p2: Prop = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  val p3: Prop = Prop.check {
    Par.equal(ES)(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p4: Prop = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES) get
  }

  val pint: Gen[Par[Int]] = Gen.choose(0, 10) map Par.unit
  val p5: Prop = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
  val forkProp: Prop = forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](gen: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gen.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def toOption: Gen[Option[A]] = Gen(sample.map(a => Option(a)))

  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def unSized: SGen[A] = SGen(_ => this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)((_, _))
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d * (j - i)))

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  val boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  val uniform: Gen[Double] = Gen(State(RNG.double))

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 != 0) n + 1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 == 0) n + 1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i <- choose(from, to)
    j <- if (i % 2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def get[A](gen: Gen[Option[A]]): Gen[A] =
    Gen(gen.sample.map(a => a.get))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  implicit def unSized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  def listOf[A](gen: Gen[A]): SGen[List[A]] = SGen(n => gen.listOfN(n))

  def listOf1[A](gen: Gen[A]): SGen[List[A]] =
    SGen(n => gen.listOfN(n max 1))

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => s => i)

  /**
   * A test for List
   */
  val smallInt: Gen[Int] = Gen.choose(-10, 10)
  val maxProp: Prop = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  /**
   * We specify that every sorted list is either Empty, has one element,
   * Or has no two consecutive elements `(a, b)` such that `a` is higher than `b`
   */
  val sortedProp: Prop = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }

  /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
 * computation for each element of the input list summed to produce the final
 * result. This is not the most compelling example, but it provides at least some
 * variation in structure to use for testing.
 *
 * Note that this has to be a `lazy val` because of the way Scala initializes objects.
 * It depends on the `Prop` companion object being created, which references `pint2`.
 */
  lazy val pint2: Gen[Par[Int]] =
    choose(-100, 100).listOfN(choose(0, 20)).map(l =>
      l.foldLeft(Par.unit(0))((p, i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))(_ + _)
        }))
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(g(_) map f)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => g(n).flatMap(f(_).g(n)))

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}

/**
 * Extractor
 */
object ** {
  def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
}

