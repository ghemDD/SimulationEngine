package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift

@lift
class ValueInvestor(val security: FundamentalSecurity, 
val initial_val: Double, 
val eagerness: Double, 
val resolution: Double) extends Actor {

  var currentPrice: Double = 0
  var currentResolution: Int = 0
  var valuation: Double = 0 
  // (Time, Magnitude)
  var event_queue = List[(Int, Double)]()

  /**
   * @return action performed by the investor : 1 signals a buy order, 0 inaction, -1 sell order 
   **/
  def act(): Int = {
    // Process the event queue
  
    for (t <- event_queue) if (t._1 == 0) valuation = valuation + t._2
    event_queue = event_queue.map(t => (t._1 - 1, t._2)).filter(t => t._1 >= 0) 

    // Decide whether to trade on the perception that the current price is off the true value.
    val laziness = (1.0 / eagerness).toInt
    val now = Random.nextInt(laziness) == 0
    if (now) math.signum(valuation - currentPrice).toInt else 0
  }


  def main(): Unit = {
    // Wait for the market to open
    while (!security.getOpen()) {
      waitLabel(Turn, 1)
    }

    println("Value Investor : Security READY !")
    
    while (currentResolution < resolution) {
      // Synchronize with the security
      while (currentResolution > security.getCurrentResolution()) {
        waitLabel(Turn , 1)
      }

      // Synchronized price
      var price = security.getCurrentPrice()

      var action = act()

      security.report(action, currentResolution)

      println("Current Resolution : "+currentResolution+" | Value Investor Action " + action)
      currentResolution = currentResolution + 1
    }
  }
}
