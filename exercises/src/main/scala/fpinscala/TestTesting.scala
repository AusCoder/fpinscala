package fpinscala

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.parallelism.Par
import fpinscala.state._
import fpinscala.testing._
import fpinscala.testing.SGen._
import fpinscala.testing.Prop._
import laziness._

object TestTesting {
  def main(args: Array[String]): Unit = {
    val str = Gen.string
    val printed = str.sample map println

    val simple3: RNG = RNG.Simple(3)
    val simple4: RNG = RNG.Simple(4)
    val simple5: RNG = RNG.Simple(5)
    printed.run(simple3)
    printed.run(simple3)
    printed.run(simple4)

    val genInt = Gen.choose(0, 100)
    val genIntPair = Gen.intPair(genInt)
    val printedIntPair = genIntPair.sample map println
    printedIntPair run simple3
    printedIntPair run simple4

    val genInt2 = Gen.choose(0,2)
    val printedInt = genInt2.sample map println
    printedInt run simple3
    printedInt run simple4
    printedInt run simple5

    val genOpt = Gen.option(genInt)
    val printedOpt = genOpt.sample map println
    printedOpt run simple3
    printedOpt run simple4
    printedOpt run simple5

    val genUnion = Gen.union(genInt, genInt2)
    val printedGenUnion = genUnion.sample map println
    printedGenUnion run simple3
    printedGenUnion run simple4
    printedGenUnion run simple5

    val genSmallInt: Gen[Int] = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(genSmallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    run(maxProp)

    val sortedProp = forAll(listOf(genInt)) { xs =>
      val s = xs.sorted
      s.headOption match {
        case None => xs == List.empty[Int]
        case Some(x) => s.forall(_ >= x)
      }
    }
    run(sortedProp)

    val es: ExecutorService = Executors.newCachedThreadPool()
    val parProp = Prop.check {
      val p1 = Par.unit(2)
      val p2 = Par.map(Par.unit(1))(_ + 1)
      p1(es).get == p2(es).get
    }
    run(parProp)

    val parProp2 = Prop.forAllPar()
  }
}
