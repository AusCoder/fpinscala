package fpinscala

import fpinscala.state._

object TestState extends App {
  val rng: RNG = RNG.Simple(1.toLong)

  println(RNG.ints_old(10)(rng))
  println(RNG.ints(10)(rng))

//  println(RNG.double(rng))
//  println(RNG.intDouble(rng))
  println(RNG.mapViaFlatMap(RNG.ints(10))(l => l.map(_*2))(rng))
}
