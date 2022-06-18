package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift

/** Unused
abstract class SingleSecurityDerivative(
    val security: Security
) extends Security {

  def estimate_stddev(
      S0: Double,
      current_time: Double,
      num_it: Int = 1000,
      resolution: Int = 100
  ): Double =
    compute_standard_deviation(
      estimate_price(S0, current_time, num_it, resolution),
      () => sample_price(S0, current_time, resolution),
      num_it)

  /** Estimate the current price of the security using sampling. */
  def estimate_price(
      S0: Double,
      current_time: Double,
      num_it: Int = 1000,
      resolution: Int = 100
  ): Double =
    expectation(() => sample_price(S0, current_time, resolution), num_it)

  def plot_price(
      S0: Double,
      current_time: Double,
      until_time: Double,
      resolution: Int,
      samples_per_tick: Int = 1000
  ): Unit = {
    val inc = (until_time - current_time) / resolution

    val xy = (for (k <- 0 to resolution) yield {
      val p = expectation(() => sample_price(S0, current_time + k * inc, k),
                          samples_per_tick)

      (current_time + k * inc, p)
    }).toArray

    val x = xy.map(_._1)
    val S = xy.map(_._2)

    plot("Price Forecast for " + this, "time", "price", x, List(S))
  }

  /** Note: The price of buying the derivative is NOT taken into account.

      This is based on simulation. Try a current_time earlier than the
      execution date and a small n to see that the textbook
      curves are, in the limit, what simulation gives us.
    */
  def plot_payoff_profile(S_from: Double,
                          S_to: Double,
                          current_time: Double,
                          n: Int = 1): Unit = {
    val resolution = 100
    val v = (for (k <- 1 to resolution) yield {
      val S0 = S_from + (S_to - S_from) * k / resolution
      (S0, expectation(() => sample_price(S0, current_time), n))
    }).toArray
    val x = v.map(_._1)
    val y = v.map(_._2)

    plot("Payoff Profile for " + this + " at time " + current_time,
         "price of security",
         "price of derivative",
         x,
         List(y))
  }

  /** current price. for futures and options this is nontrivial. */
  def sample_price(
      S0: Double,
      current_time: Double,
      resolution: Int = 1
  ): Double =
    sample_future_price(S0, current_time, current_time, resolution)
} **/