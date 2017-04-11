package fpinscala

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object TestTask {
  def main(args: Array[String]): Unit = {
    var depth = 0

    def f(x: Int): Int = {
      depth += 1
      println(s"depth: $depth")
      f(x+1)+1
    }

    def x: Task[Unit] = Task {
      depth += 1
      println(s"depth: $depth")
    }.flatMap(_ => x)

    x.unsafePerformSync
  }
}
