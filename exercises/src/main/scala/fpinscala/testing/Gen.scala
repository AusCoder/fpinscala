package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/*
sum: List[Int] -> Int should satisfy:
  - for any permutation p of the list l, sum(p . l) == sum(l)
  - sum (n * [m]) == n*m
  - sum(l) + sum(k) == sum(l ++ k)
 */

sealed trait Result {
  def isFailed: Boolean = this match {
    case Passed => false
    case Falsified(_, _, _) => true
    case Proved => false
  }
}
case object Passed extends Result
case object Proved extends Result
case class Falsified(failed: FailedCase, success: SuccessCount, tags: List[String]) extends Result

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(rhs: Prop): Prop = Prop {
      (m: MaxSize, n: TestCases, rng: RNG) => {
        val thisResult = this.run(m, n, rng)
        thisResult match {
          case Passed => rhs.run(m, n, rng) match {
            case Passed => Passed
            case Falsified(f, s, l) => Falsified(f, s, "right hand side failed" :: l)
            case Proved => Passed
          }
          case Proved => {
            rhs.run(m, n, rng) match {
              case Passed => Passed
              case Falsified(f, s, l) => Falsified(f, s, "right hand side failed" :: l)
              case Proved => Proved
            }
          }
          case Falsified(f, s, l) => Falsified(f, s, "left has side failed" :: l)
        }
      }
    }
  def ||(rhs: Prop): Prop = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  /* forAll for a standard Gen, ignores max case size */
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => {
      randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
        case (a, idx) => {
          try {
            if (f(a)) Passed else Falsified(s"failed case: $a", idx, List.empty[String])
          }
          catch { case e: Exception => Falsified(buildMsg(a, e), idx, List.empty[String]) }
        }
      }.find(_.isFailed).getOrElse(Passed)
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) => {
      // always at least one case case, and approx equal test cases per size
      val casesPerSize: Int = (n + m - 1) / m
      // get props of all sizes up to m, but no more props that number of test cases
      val props: Stream[Prop] = Stream.from(0).take((n min m) + 1).map(size => forAll(g(size))(f))
      // for each prop, run with num cases for that prop, reduce them to single prop.
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      // run the prop
      prop.run(m, n, rng)
    }
  }

  val S: Gen[ExecutorService] = Gen.weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](gen: Gen[A])(f: A => Par[Boolean]): Prop = {
    val g  = Gen.map2(S, gen)((_, _))
    forAll(g) {
      case (es, a) => f(a)(es).get
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace: ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
         maxSize: Int = 100,
         testCases: Int = 100,
         rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n, _) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, pass $testCases tests.")
      case Proved =>
        println(s"+ OK, property proved.")
    }
  }

// TODO: prove properties with a finite domain.
  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0, List.empty)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val state: State[RNG, B] = this.sample.flatMap(a => f(a).sample)
    Gen(state)
  }
  def map[B](f: A => B): Gen[B] = {
    this.flatMap(a => Gen.unit(f(a)))
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => {
      val states: List[State[RNG, A]] = List.fill(n)(this.sample)
      Gen(State.sequence(states))
    })
  }
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val rng = RNG.map(RNG.nonNegativeLessThan(stopExclusive - start))(_ + start)
    Gen(State(rng))
  }
  def boolean: Gen[Boolean] = {
    val r = RNG.map((rng: RNG) => rng.nextInt)(x => if (x%2 == 0) true else false)
    Gen(State(r))
  }
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val samps: List[Gen[A]] = List.fill(n)(g)
    Gen.sequence(samps)
  }
  def list[A](g: Gen[A]): Gen[List[A]] = {
    val state: State[RNG, List[A]] =
      Gen.choose(0, 100).sample.flatMap(n => listOfN(n, g).sample)
    Gen(state)
  }
  def sequence[A](l: List[Gen[A]]): Gen[List[A]] = {
    val inter = l.map(_.sample)
    Gen(State.sequence(inter))
  }
  def char: Gen[Char] = {
    val r = RNG.map((rng: RNG) => RNG.nonNegativeInt(rng))(_%26)
    val s = RNG.map(r)(x => ('a' to 'z').toList(x))
    Gen(State(s))
  }
  def string: Gen[String] = {
    val x: State[RNG, String] = list(char).sample.map(l => l.foldRight("")(_ + _))
    Gen(x)
  }
  def map2[A,B,C](g1: Gen[A], g2: Gen[B])(f: (A,B) => C): Gen[C] = {
    val state: State[RNG, C] = g1.sample.map2(g2.sample)(f)
    Gen(state)
  }
  def pair[A](g: Gen[A]): Gen[(A, A)] = {
    map2(g,g)((_, _))
  }
  def intPair(g: Gen[Int]): Gen[(Int, Int)] = {
    pair(g)
  }
  def option[A](g: Gen[A]): Gen[Option[A]] = {
    map2(Gen.choose(0,2), g) {
      case (n, x) => {
        if (n == 0) None
        else Some(x)
      }
    }
  }
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen.choose(0,2).flatMap {
      case 0 => g1
      case 1 => g2
    }
  }
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val total = g1._2 + g2._2
    val fract = Gen.choose(0, 100).map(_.toDouble).map(_ / 100).map(_ * total)
    fract.flatMap {
      case x if x < g1._2 => g1._1
      case _              => g2._1
    }
  }
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen(n => this.forSize(n).flatMap(a => f(a).forSize(n)))
  }
  def map[B](f: A => B): SGen[B] = {
    SGen(n => this.forSize(n).map(a => f(a)))
  }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    def go(n: Int): Gen[List[A]] = Gen.sequence(List.fill(n)(g))
    SGen(go)
  }
  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    def go(n: Int): Gen[List[A]] = for {
      a <- g
      l <- listOf(g).forSize(n)
    } yield a :: l
    SGen(go)
  }
}

//trait SGen[+A] {
//
//}

