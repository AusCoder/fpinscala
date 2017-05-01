package fpinscala

import fpinscala.laziness._

object TestStream {
  def main(args: Array[String]): Unit = {

    Stream.ones.flatMap(x => Stream(x)).take(5).toList.foreach(println)
  }
}
