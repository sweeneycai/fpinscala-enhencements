package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  /**
   * Generate a unit of Rand with type A
   *
   * @param a An instance of A
   * @tparam A Type A
   * @return A unit of Rand with type A
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    // consider when i = Int.MinValue
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue + 1), r)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (l, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((l, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      // g: A => Rand[B] expand g: A => RNG => (B, RNG)
      g(a)(r1) // We pass the new state along
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def _map[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))
}

import fpinscala.state.State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  // 这个地方要注意`unit`生成的类型
  def sequenceViaFoldRight[S, A](f: List[State[S, A]]): State[S, List[A]] =
    f.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))

  def sequence[S, A](f: List[State[S, A]]): State[S, List[A]] = {
    @scala.annotation.tailrec
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, acc.::(a))
        }
      }

    State(s => go(s, f, List[A]()))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

// TODO
object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  def update: Input => Machine => Machine = (i: Input) => (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
  }
}