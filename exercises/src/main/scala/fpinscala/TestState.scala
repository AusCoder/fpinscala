package fpinscala

import fpinscala.state._

object TestState extends App {
  val rng: RNG = RNG.Simple(1.toLong)

  println(RNG.ints_old(10)(rng))
  println(RNG.ints(10)(rng))

//  println(RNG.double(rng))
//  println(RNG.intDouble(rng))
  println(RNG.mapViaFlatMap(RNG.ints(10))(l => l.map(_*2))(rng))

  val m: Machine = Machine(true, 5, 10)
  val inputs: List[Input] = List(Coin, Turn, Coin, Turn, Turn, Turn, Coin, Coin, Coin, Turn)
  val (t, s) = State.simulateMachine(inputs).run(m)
  println(t)
}
