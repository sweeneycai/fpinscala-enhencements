package fpinscala.parallelism

import java.util.concurrent._


object Par {
  // Par is a function, it accepts an ExecutorService and return a Future
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] =
    a(s) // just call the function of Par

  def unit[A](a: A): Par[A] =
    (_: ExecutorService) =>
      UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
  // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
  // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    es => {
      val af = a(es)
      val bf = b(es)
      val cf = c(es)
      UnitFuture(f(af.get, bf.get, cf.get))
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es =>
      es.submit(new Callable[A] {
        def call: A = a(es).get
      })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // Make an async operation, make a function async
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => fork(unit(f(a)))

  // Put a: Par[A] into a separate thread
  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def asyncFViaLazyUnit[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  // Like Future.sequence, this method can convert a List[Par[A]] to Par[List[A]]
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((a: Par[A], b: Par[List[A]]) => map2(a, b)(_ :: _))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF(a => if (f(a)) List(a) else List())
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get)
        t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // choose the n'th Par
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(a => if (a) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val ind = run(es)(key).get
      run(es)(choices(ind))
    }

  def chooser[A, B](par: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val pa = run(es)(par).get
      run(es)(choices(pa))
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(a => if (a) t else f)

  def choiceNViaChooser[A](par: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(par)(a => choices(a))

  def choiceMapViaChooser[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(k => choices(k))

  /* chooser is usually called flatMap */
  def flatMap[A, B](par: Par[A])(choice: A => Par[B]): Par[B] =
    es => {
      val pa = run(es)(par).get
      run(es)(choice(pa))
    }

  def join[A](par: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(par).get())

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  def flatMapViaJoin[A, B](par: Par[A])(f: A => Par[B]): Par[B] =
    join(map(par)(f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {}

}

object Examples {
  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library.
  // Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  //  def max(ints: IndexedSeq[Int]): Int =
  //    if (ints.size <= 1)
  //      ints.headOption getOrElse 0
  //    else {
  //      implicit val executorService
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      val maxL: Par[Int] = Par.unit(max(l))
  //      val maxR: Par[Int] = Par.unit(max(r))
  //      if (Par.delay(maxL) > Par.delay(maxR)) Par.delay(maxL)
  //      else Par.run()(maxR)
  //    }

  def main(args: Array[String]): Unit = {
    println(sum(IndexedSeq(1, 2, 3, 4, 5, 7)))
  }
}
