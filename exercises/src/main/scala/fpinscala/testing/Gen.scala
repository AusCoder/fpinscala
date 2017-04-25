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
    case Falsified(_, _) => true
  }
}
case object Passed extends Result
case class Falsified(failed: FailedCase, success: SuccessCount) extends Result

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(rhs: Prop): Prop = Prop {
    (n, rng) => {
      val thisResult = this.run(n, rng)
      if (thisResult.isFailed) thisResult else rhs.run(n, rng) // modify failed string to include which side failed.
    }
  }
  def ||(rhs: Prop): Prop = ???
}

//trait Prop {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//  def &&(rhs: Prop): Prop = if (this.check.isLeft) rhs else this
//}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => {
      randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
        case (a, idx) => {
          try {
            if (f(a)) Passed else Falsified(s"failed case: $a", idx)
          } catch { case e: Exception => Falsified(buildMsg(a, e), idx) }
        }
      }.find(_.isFailed).getOrElse(Passed)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace: ${e.getStackTrace.mkString("\n")}"
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val state: State[RNG, B] = this.sample.flatMap(a => f(a).sample)
    Gen(state)
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => {
      val states: List[State[RNG, A]] = List.fill(n)(this.sample)
      Gen(State.sequence(states))
    })
  }
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
    val samps: List[Gen[A]] = (1 to n).toList.map(_ => g)
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
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

