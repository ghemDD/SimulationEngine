package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._

trait Security {
  def compute_time_series(
      S0: Double, // start_price
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Array[Double]

  /** The price of the security at future point.
      The parameters are as for compute_time_series().
    */
  def sample_future_price(
      S0: Double,
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Double = {
    assert(current_time <= goal_time)
    compute_time_series(S0, current_time, goal_time, resolution)(resolution)
  }

  /** Volatility */
  //def stddev(): Double
}

trait InteractiveSecurity extends Security {
    // Method called by agents to report their action
    //def report(action: Int, resolution: Int): Boolean

    // Aggregates the agents action for the current turn
    def aggregate(): Unit 

    // Returns the current resolution of the security
    def getCurrentResolution(): Int

    // Returns if the security is ready 
    def getOpen(): Boolean

    // Returns the current price of the security
    def getCurrentPrice(): Double
}