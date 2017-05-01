package fpinscala

import fpinscala.laziness._

object TestStream extends App {
//  val xs = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
//  val ys = Stream(10,20,30,40,50)
//  val zs = Stream(1,2,3,4)
//  val ws = Stream(2,3,4)
//  val vs = Stream()
//  val us = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
//  val emp = Stream()
//
//  def nNTimes(n: Int): Stream[Int] = (1 to n).foldRight(Empty: Stream[Int])((_, s) => Stream.cons(n, s))
//
//  println(Stream.add(xs, ys).toList)
//
//  println(xs.flatMap(nNTimes(_)).toList)
//
//  println("fun with infinite streams!")
//  val ones: Stream[Int] = Stream.cons(1, ones)
//  println(ones.take(10).toList)
//  println(ones.map(_+1).exists(_%2 == 0))
//  println(ones.exists(_ == 1))
//
//  println("from fun!")
//  println(Stream.from(7).take(10).toList)
//
//  println("ones fun!")
//  println(Stream.ones2.take(17).toList)
//
//  println("fib fun!")
//  println(Stream.fibs.take(10).toList)
//  println(Stream.fibs2.take(10).toList)
//
//  println("unfold fun!")
//  println(xs.map2(_*2).toList)
//  println(xs.take2(0).toList)
//  println(xs.take2(1).toList)
//  println(xs.take2(2).toList)
//  println(xs.take2(3).toList)
//  println(xs.takeWhile2(_<5).toList)
//
//  println("startsWith fun!")
//  println(xs startsWith ys)
//  println(xs startsWith zs)
//  println(xs startsWith ws)
//  println(xs startsWith vs)
//  println(xs startsWith us)

  Stream.from(1).take2(3000000).toListTailRec.foreach(println)
}