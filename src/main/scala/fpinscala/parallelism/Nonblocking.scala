package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import scala.language.higherKinds

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(par: Par[A]): A = {
      // A mutable, threadsafe reference, to use for storing the result
      val ref = new java.util.concurrent.atomic.AtomicReference[A]()
      // When a latch decremented, implies that `ref` which has the result
      val latch = new CountDownLatch(1)
      // Asynchronously set the result, and decrement the latch
      par(es) { a => ref.set(a); latch.countDown() }
      // Block until the `latch.countDown` is invoked asynchronously
      latch.await()
      // Once we have passed the latch, we know `ref` has been set, and return its value
      ref.get()
    }

    // 这个地方可以直接使用lambda表达式，这是因为我们定义的Par类型可以通过自动推断推断出各个位置的类型
    def unit[A](a: A): Par[A] = _ => (cb: A => Unit) => cb(a)

    def delay[A](a: => A): Par[A] = _ => (cb: A => Unit) => cb(a)

    def fork[A](par: => Par[A]): Par[A] = es => (a: A => Unit) => eval(es)(par(es)(a))

    /**
     * A helper function for evaluating an action asynchronously,
     * using the given `ExecutorService`
     *
     * @param es Given `ExecutorService`
     * @param r  Action
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        override def call(): Unit = r
      })

    /**
     * Make an action async, helper functions for constructing `Par` values out of calls to
     * non-blocking continuation-passing-style APIs.
     *
     * @param f Action
     * @tparam A Type A
     * @return Par[A]
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = _ => (k: A => Unit) => f(k)

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => (k: C => Unit) => {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(k(f(a, br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(k(f(ar.get, b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }

    def map[A, B](par: Par[A])(f: A => B): Par[B] =
      es =>
        (k: B => Unit) => par(es)(a => eval(es)(k(f(a))))

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    /**
     * Make a function async
     *
     * @param f Function
     * @tparam A type A
     * @tparam B type B
     * @return Async function
     */
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, sequenceRight(t))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => (k: A => Unit) => p(es) {
        b =>
          if (b) eval(es) {
            t(es)(k)
          }
          else eval(es) {
            f(es)(k)
          }
      }

    def choiceN[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => (k: A => Unit) => p(es) {
        i =>
          eval(es) {
            choices(i)(es)(k)
          }
      }

    def choiceViaChoiceN[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(p)(a => if (a) 0 else 1))(List(t, f))

    def choiceMap[K, V](p: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => (k: V => Unit) => p(es)(ind => choices(ind)(es)(k))

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => (k: B => Unit) => p(es)(a => f(a)(es)(k))

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    def choiceViaFlatMap[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(p)(b => if (b) t else f)

    def join[A](p: Par[Par[A]]): Par[A] =
      es => (k: A => Unit) => p(es)(a => a(es)(k))

    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))

    def joinViaFlatMap[A](p: Par[Par[A]]): Par[A] =
      flatMap(p)(a => a)

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    case class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)

      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)

      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
    }

  }

}