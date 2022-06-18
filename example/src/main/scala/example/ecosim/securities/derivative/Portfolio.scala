package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift


class Portfolio(
    securities: List[Security]
) extends Security {

  // TODO chart: this only requires summing up
  def compute_time_series(
      S0: Double, // start_price
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Array[Double] = {
    assert(false)
    new Array[Double](0)
  }

  override def sample_future_price(
      S0: Double,
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Double =
    securities
      .map(sec =>
        sec.sample_future_price(S0, current_time, goal_time, resolution))
      .sum

  /** Not implemented. */
  def stddev: Double = { assert(false); 0.0 }
}