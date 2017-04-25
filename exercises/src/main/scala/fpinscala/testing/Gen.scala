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

trait Prop {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def check: Boolean

  def &&(rhs: Prop): Prop = if (this.check) rhs else this
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A])

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
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

