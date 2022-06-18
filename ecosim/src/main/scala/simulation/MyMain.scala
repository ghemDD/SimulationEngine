package ecosim.simulation

import ecosim.securities.Commodities.{Flour, Land, Wheat}
import ecosim.simulation.simlib._

import scala.collection.immutable

object MyMain {
  val s = new Simulation

  val f = new Farm(s)
  val m = new Mill(s)
  //val c   = new Cinema(s);
  //val rf  = new CattleFarm(s);
  //val mcd = new McDonalds(s);
  val landlord = new Source(Land, 20, 100000 * 100, s)
  //val freudensprung = new Source(Beef,   100,  26000*100, s);
  //val silo          = new Source(Wheat, 1000,   6668*100, s);
  //val silo2         = new Trader(Whear, 100, s);
  //val billa         = new Trader(Flour, 50, s);
  val mehlbuyer = Buyer(Flour, () => 40, s)

  val people: immutable.IndexedSeq[Person] = for (_ <- 1 to 12)
    yield new Person(s, false)

  s.init(
    List(
      landlord,
      //silo,
      // silo2, billa, freudensprung,
      f,
      m,
      // c, rf, mcd,
      mehlbuyer
    ) ++ people.toList)

  def main(argv: Array[String]) = {
    if ((argv.length != 1) || (argv(0).toInt < 1))
      println("Exactly one integer >0 argument needed!")
    else
      s.run(argv(0).toInt)
  }
}

object TradingExample extends App{

  val simu = new Simulation

  val s = new Source(Wheat, 4, 1000 * 100, simu)
  val t = Trader(Wheat, 1, simu)
  val b = simlib.Buyer(Wheat, () => 1, simu)

  simu.init(List(s, t, b))
  simu.run(4)
  /* After 4 steps we have
(BalanceSheet(4000,4000,4000,0,0),ArrayBuffer(getreide -> 0@0))
(BalanceSheet(50,50,50,0,0),ArrayBuffer(getreide -> 0@1000))
(BalanceSheet(0,4050,0,-4050,0),ArrayBuffer(getreide -> 4@1012))
 */
}
