package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._


class EuropeanCallOption(
    security: Security,
    strike_price: Double,
    due_date: Double,
    risk_free_rate: Double
) extends Security {

  // TODO chart: we could do all kinds of things here, but the interesting
  // prices to be charted here are at which the derivative is TRADED.
  // We only know this by running a market.
  override def compute_time_series(
      S0: Double, // start_price
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Array[Double] = {
    security.compute_time_series(S0, current_time, goal_time, resolution)
  }

  /** Uses protected abstract function `value_at_expiration_time`, which
      is specific to the derivative.

    @param S0            current price of the underlying security
    @param current_time  the current time at which S0 is the prict.
                         must not be greater than `due_date`.
    @param goal_time     the time at which the price is to be queries.
                         This is in general not the expiration date.
                         may be greater or smaller than `due_date`.
    @param resolution    used for sampling future price of underlying
                         security.
    */
  override def sample_future_price(
      S0: Double,
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Double = {
    assert(current_time <= due_date)

    // The price of the underlying security at expiration time
    val sec_S = security.sample_future_price(S0, current_time, due_date, resolution)

    // The value of the derivative at expiration time
    val drv_S = value_at_expiration_time(sec_S)

    /** What is the current value of being able to sell the derivative at the
        expiration time at price drv_S?**/
    drv_S * math.exp(risk_free_rate * (goal_time - due_date))
    // Same as:
    // compound_interest(risk_free_rate, drv_S, goal_time - due_date)
  }

  // TODO: Implement
  def stddev: Double = { assert(false); 0.0 }

  /** Compute the current price of the option using the Black-Scholes formula.
      Must only be used for VanillaSecurities.
      @param S0 security price
  **/
  def BlackScholes(S0: Double, current_time: Double): Double = {
    val sec = security.asInstanceOf[VanillaSecurity]
    val r = sec.r; // risk-free interest rate
    val stddev = sec.stddev; // standard deviation of the security
    val K = strike_price; // option strike price
    val T = due_date - current_time; // time to expiration

    assert(T >= 0)

    def N(d: Double) =
      breeze.stats.distributions.Gaussian(0, 1).probability(-1.0 / 0, d)

    val nrm = stddev * math.sqrt(T)
    val d = (math.log(S0 / K) + (r + 0.5 * stddev * stddev) * T) / nrm

    S0 * N(d) - K * math.exp(-r * T) * N(d - nrm)
  }


  /** Value at expiration time of the derivative as a function of the
      price `S` of the underlying security at expiration time.
    */
  protected def value_at_expiration_time(S: Double): Double =
    math.max(0, S - strike_price)
}
