package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift

abstract class FutureOption(security: Security, 
strike_price: Double,
due_date: Double, 
risk_free_rate: Double) extends Security {

// TODO chart: we could do all kinds of things here, but the interesting
  // prices to be charted here are at which the derivative is TRADED.
  // We only know this by running a market.
  override def compute_time_series(
      S0: Double, // start_price
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Array[Double] = {
    assert(false)
    new Array[Double](0)
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

    // the price of the underlying security at expiration time
    val sec_S =
      security.sample_future_price(S0, current_time, due_date, resolution)

    // the value of the derivative at expiration time
    val drv_S = value_at_expiration_time(sec_S)

    // what is the current value of being able to sell the derivative at the
    // expiration time at price drv_S?
    drv_S * math.exp(risk_free_rate * (goal_time - due_date))
    // same as:
    // compound_interest(risk_free_rate, drv_S, goal_time - due_date)
  }

  // TODO: Implement
  def stddev: Double = { assert(false); 0.0 }

  /** Value at expiration time of the derivative as a function of the
      price `S` of the underlying security at expiration time.
    */
  protected def value_at_expiration_time(S: Double): Double
}