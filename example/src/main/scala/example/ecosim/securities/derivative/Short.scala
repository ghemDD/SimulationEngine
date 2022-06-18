package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift

case class Short(
    val security: Security
) extends Security {

  override def compute_time_series(
      S0: Double,
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Array[Double] = {
    security
      .compute_time_series(S0, current_time, goal_time, resolution)
      .map(x => -x)
  }
    
  override def sample_future_price(
      S0: Double,
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Double = {
      - security.sample_future_price(S0, current_time, goal_time, resolution)
  }
    

  //def stddev: Double = security.stddev
}