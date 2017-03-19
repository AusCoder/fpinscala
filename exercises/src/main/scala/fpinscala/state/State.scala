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

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = if (rng.nextInt._1 < 0) (rng.nextInt._1 + Integer.MAX_VALUE + 1, rng.nextInt._2) else rng.nextInt

  def double: Rand[Double] = map(nonNegativeInt)(x => x.toDouble / Integer.MAX_VALUE)

  def double_old(rng: RNG): (Double, RNG) = {
    val (int, seed) = nonNegativeInt(rng)
    (int.toDouble / Integer.MAX_VALUE, seed)
  }

  def intDouble_old(rng: RNG): ((Int,Double), RNG) = {
    val (int, seed) = rng.nextInt
    ((int, double(rng)._1), seed)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    both(nonNegativeInt, double)(rng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (pair, seed) = intDouble(rng)
    ((pair._2, pair._1), seed)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

  def ints_old(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List.empty[Int], rng)
    else {
      val (x, s1) = rng.nextInt
      val (xs, s2) = ints_old(count-1)(s1)
      (x :: xs, s2)
    }
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(rng => rng.nextInt))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng_a) = ra(rng)
    val (b, rng_b) = rb(rng_a)
    (f(a,b), rng_b)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.headOption match {
      case None => (List.empty[A], rng)
      case Some(rand) => {
        val (a, rng1) = rand(rng)
        val (as, rng2) = sequence(fs.tail)(rng1)
        (a :: as, rng2)
      }
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(x => {
    val mod = x % n
    if (x + (n-1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  })

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
