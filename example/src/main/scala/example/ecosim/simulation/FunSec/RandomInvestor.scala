package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift

@lift
class RandomInvestor(val security: FundamentalSecurity, 
val resolution: Double) extends Actor {
  var currentPrice: Double = 0
  var currentResolution: Int = 0

  /**
   * @return random action performed by the investor : 1 signals a buy, 0 inaction, -1 sell order
   **/
  def act(): Int = Random.nextInt(3) - 1

  def main(): Unit = {

    println("Random Investor : Security NOT READY !")

    while(!security.getOpen()) {
      waitLabel(Turn, 1)
    }

    println("Random Investor : Security READY !")

    while (currentResolution < resolution) {

      // Synchronize with security
      while (currentResolution > security.getCurrentResolution()) {
        waitLabel(Turn , 1)
      }

      var price = security.getCurrentPrice()
      var action: Int = act()

      security.report(action, currentResolution)

      println("Current Resolution : " + currentResolution + " | Random Investor Action = " + action)
      currentResolution = currentResolution + 1
    }
  }
}
