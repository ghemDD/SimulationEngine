package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._

object util {
  /** Used to plot the variation of prices graph 
   * 
   * @param graphName 
   * @param abscissa
   * @param ordinal 
   * @param resolution
   * @param y
   **/
  def plot(graphName: String,
           abscissa: String,
           ordinal: String,
           resolution: Int,
           y: List[Array[Double]]): Unit = {

    // Time
    val x = new Array[Double](resolution)
    var i = 0
    while (i < resolution) {
      x(i) = i.toDouble
      i = i + 1
    }

    val x1 = DenseVector(x)
    val y1 = y.map(DenseVector(_))

    val f = Figure(graphName)
    val p = f.subplot(0)
    p.xlabel = abscissa
    p.ylabel = ordinal
    for (ys <- y1) p += breeze.plot.plot(x1, ys)
    f.refresh
  }

  def createPlot(graphName: String): Figure = {
    Figure(graphName)
  }

  def updatePlot(graphName: String,
           abscissa: String,
           ordinal: String,
           resolution: Int,
           y: List[Array[Double]],
           f: Figure): Unit = {
      // Time
      val x = new Array[Double](resolution)
      var i = 0
      while (i < resolution) {
        x(i) = i.toDouble
        i = i + 1
      }

      val x1 = DenseVector(x)
      val y1 = y.map(DenseVector(_))
      val p = f.subplot(0)
      p.xlabel = abscissa
      p.ylabel = ordinal
      for (ys <- y1) p += breeze.plot.plot(x1, ys)
      f.refresh
    }

  /** Geometric Brownian Motion. For a security in the Black-Scholes Model,
    this needs to be scaled by (multiplied with) the starting price of the
    security. This approximates continuous Brownian Motion for dt time.
    @param mu risk-free rate
    @param sigma volatility
    @param dt time to run
    @return geometric Browmian motion for corresponding interval time dt
  **/
  def geoBM(
      mu: Double,
      sigma: Double,
      dt: Double
  ): Double = {
    math.exp(
      (mu - sigma * sigma / 2) * dt
        + sigma * Nsample(0, math.sqrt(dt)))
  }

  /**
   * @param mu
   * @param sigma
   * @return Gaussian distribution 
   **/
  def Nsample(mu: Double, sigma: Double): Double =
    breeze.stats.distributions.Gaussian(mu, sigma).sample(1)(0)

  /**
   * @param sample
   * @param n
   * @return 
   **/
  def expectation(sample: () => Double, n: Int): Double = {
    (for (_ <- 1 to n) yield sample()).sum / n
  }

  /**
   * @param mean
   * @param sample
   * @param n
   * @return 
   **/
  def compute_standard_deviation(
      mean: Double,
      sample: () => Double,
      n: Int
  ): Double =
    math.sqrt((for (_ <- 1 to n) yield {
      val d = mean - sample()
      d * d
    }).sum) / (n - 1)

      /** given the risk free rate, and a value S, get compounded value after
      time delta_t.

      @param delta_t is in years
      @return 
    */
  def compound_interest(risk_free_rate: Double,
                        S: Double,
                        delta_t: Double): Double =
    S * math.exp(risk_free_rate * delta_t)

  // argument of sample is # of samples to take; produces a vector
  /**
   * @param label 
   * @param xlabel 
   * @param sample
   * @param num_samples
   **/
  def plot_distribution(
      label: String,
      xlabel: String,
      sample: () => Int,
      num_samples: Int
  ): Unit =
     {
    val v = for (_ <- 1 to num_samples) yield sample()
    val m = v.groupBy((x: Int) => x).mapValues(_.length)

    val x0 = (m.min._1 to m.max._1).toArray
    val x = x0.map(_.toDouble)
    val y = x0.map((v: Int) => m.getOrElse(v, 0).toDouble)

    plot("Distribution Plot for " + label, xlabel, "#", x.size, List(y))
  }
}
