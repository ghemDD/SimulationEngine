package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift

class FundamentalSecurity0(
    r: Double,
    stddev: Double,
    frequency: Integer,
    amplitude: Double
) extends Security {

/** Computes the sequence of actual prices at resolution */
  override def compute_time_series(
      S0: Double, // start_price
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Array[Double] = {
    assert(current_time <= goal_time)
    assert(resolution >= 1)

    val dt = goal_time - current_time; // look this far into the future
    val S = new Array[Double](resolution + 1)
    S(0) = S0

    for (i <- 1 to resolution)
      S(i) = S(i - 1) * util.geoBM(r, stddev, dt / resolution)
    + fundamental_value_change()

    S
  }

  private def fundamental_value_change(): Double = {
    val rnd = scala.util.Random

    // do we have an event now?
    val now = (rnd.nextInt % frequency) == 0
    val magnitude = (rnd.nextDouble * 2 - 1) * amplitude

    if (now) magnitude else 0.0
  }
}