package ecosim.simulation

import ecosim.global
import ecosim.markets.{OrderBook, SellersMarket}
import ecosim.owner.Seller
import ecosim.securities.Commodities.{Commodity, all_commodities}
import ecosim.securities.Security
import ecosim.simulation.factory.Factory

import scala.collection.mutable

class Simulation {
  val market: mutable.Map[Commodity, SellersMarket] =
    collection.mutable.Map[Commodity, SellersMarket]()
  var timer = 0
  for (c <- all_commodities) {
    market += (c -> new SellersMarket(c))
  }
  var arbeitsmarkt
    : mutable.Stack[SimO] = collection.mutable.Stack[SimO](); // all Persons

  var chicago: mutable.Map[Security, OrderBook] =
    collection.mutable.Map[Security, OrderBook]()

  /** TODO: We should have a registry of sims here, which can be looked up by
    *id. This eliminates the need for substitution when copying a simulation,
    *which is a mess.
    */
  var sims: List[SimO] = List[SimO]()

  /** This is not a constructor since we first need to create the Simulation
      to hand it over to the sims, and then hand
      the sims to the Simulation (via init).

      init() accepts the list of sims `_sims`,
      enters persons into the labor market,
      and output the status of each sim.
    */
  def init(_sims: List[SimO]): Unit = {
    assert(timer == 0)
    println("INIT Simulation " + this)
    sims = _sims
    for (s <- sims) if (s.isInstanceOf[Person]) arbeitsmarkt.push(s)

    if (!global.silent) {
      for (s <- sims) { s.stat(); }
      println; println
    }

    println("INIT Simulation complete " + this)
  }

  /** To be used to start a nested simulation. Callable from the sims.

      Returns a mapping from the sims of the old simulation to those of the
      new.
    */
  def run_sim(it: Int): collection.mutable.Map[SimO, SimO] = {
    val (new_sim, old2new) = this.mycopy()

    // prevent recursive simulation. This is only safe it the simulation
    // runs for fewer than 1000 iterations!
    for (s <- new_sim.sims)
      s match {
        case factory: Factory => factory.prev_mgmt_action += 1000
        case _                =>
      }

    new_sim.run(it)
    old2new
  }

  /** TODO: Object ids (owners) in logs don't get substituted yet.
      This will become necessary when we want to compute supply by _other_
      sellers.
    */
  def mycopy(): (Simulation, mutable.Map[SimO, SimO]) = {
    val s2 = new Simulation
    val old2new = collection.mutable.Map[SimO, SimO]()

    // this separation would not be needed if we had a central map from sim ids
    // to sims.
    for (s <- sims) {
      if (s.isInstanceOf[Person]) {
        val cp = s.mycopy(s2, old2new)
        old2new += (s -> cp)
      }
    }

    s2.sims = sims.map((s: SimO) => {
      old2new.getOrElse(s, {
        val cp = s.mycopy(s2, old2new)
        old2new += (s -> cp)
        cp
      })
    })

    s2.arbeitsmarkt = arbeitsmarkt.map((x: SimO) => old2new(x))

    for ((commodity, ma) <- market) {
      ma.copy_state_to(s2.market(commodity),
                       (s: Seller) => old2new(s.asInstanceOf[SimO]))
    }

    assert(s2.chicago.isEmpty)
    for ((security, ob) <- chicago) {
      s2.chicago += (security -> ob.mycopy())
    }

    s2.timer = timer

    (s2, old2new)
  }

  def run(steps: Int): Unit = {
    run_until(timer + steps - 1)
  }

  /** run the simulation. Must init() first! */
  def run_until(until: Int): Unit = {
    println("RESUME Simulation " + this)
    while (timer <= until) {
      if (!global.silent) println("timer = " + timer)
      for (s <- sims) s.run_until(timer)
      if (!global.silent) {
        for (s <- sims) s.stat()
        println(); println()
      }
      timer += 1
    }
    println("STOP Simulation " + this)
  }
}
