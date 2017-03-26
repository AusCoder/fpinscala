package fpinscala

import java.util.concurrent.Executors

import fpinscala.parallelism.Par._

object TestPar extends App {
  // an example where our Par library hits a deadlock:
  val a = lazyUnit(1)
  val executorService = Executors.newFixedThreadPool(1)
  println(equal(executorService)(a, fork(a)))
//  println(map2(a, fork(a))(_ + _)(executorService).get)

  executorService.shutdown()
}
