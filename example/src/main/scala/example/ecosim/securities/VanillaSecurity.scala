package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift


@lift
class VanillaSecurity(val resolution: Int, val init_price: Double) extends Actor 
with Security {
    var currentResolution: Double = 0
    var timedPrice: Array[Double] = null
    var time: Array[Double] = null
    var currentPrice: Double = 0
    var r: Double = 0
    var stddev: Double = 0

    // Asynchronous operations 
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

        S
    }

    // this is redundant but more efficient than super.sample_future_price
    override def sample_future_price(
      S0: Double,
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
    ): Double = {
        assert(current_time <= goal_time)
        S0 * util.geoBM(r, stddev, goal_time - current_time)
    }

  
    def main(): Unit = {
        timedPrice = new Array[Double](resolution)
        time = new Array[Double](resolution)
        currentPrice = init_price

        while (currentResolution < resolution) {
            currentPrice = currentPrice * util.geoBM(r, stddev, resolution)
            currentResolution = currentResolution + 1
            waitLabel(Turn, 1)
        }
    }
}