package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift

/**
 * 
 * 
 **/
@lift
class TechnicalInvestor(val security: FundamentalSecurity, 
  val laziness: Int, 
  val sensitivity: Double, 
  val init_price: Double, 
  val resolution: Int) extends Actor {

  var currentPrice: Int = 0
  // Price buffer
  var mem: Array[Double] = null
  var rrobin: Int = 0
  var currentResolution: Int = 0

  /**
   * @return action performed by the investor : 1 signals a buy order, 0 inaction, -1 sell order
   **/
  def act(): Int = {
    val avg = mem.sum / 10
    //val avg = 1
    var r: Int = 0

    if (rrobin % laziness == 0) {
      if (currentPrice > (1 + sensitivity) * avg) r = 1
      if (currentPrice < (1 - sensitivity) * avg) r = -1
    }

    mem(rrobin % 10) = currentPrice
    rrobin = rrobin + 1
    r
  }

  def main(): Unit = {
    // Initialize price memory buffer
    var i = 0
    mem = new Array[Double](10)
    while (i < 10) {
      mem(i) = init_price
      i = i + 1
    }

    println("Technical Investor : Security NOT READY !")

    while(!security.getOpen()) {
      waitLabel(Turn, 1)
    }

    println("Technical Investor : Security READY !")
    while(currentResolution < resolution) {
      while (currentResolution > security.getCurrentResolution()) {
        waitLabel(Turn , 1)
      }

      var price = security.getCurrentPrice()
      var action = act()

      security.report(action, currentResolution)

      println("Current Resolution : " + currentResolution + " | Technical Investor Action = " + action)
      waitLabel(Turn, 1)
      currentResolution = currentResolution + 1
    }
  }
}
