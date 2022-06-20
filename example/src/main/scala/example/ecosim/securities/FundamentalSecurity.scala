package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._


@lift
class FundamentalSecurity(val initial_price: Double, 
val numberActors: Int, 
val resolution: Int) extends Actor 
with InteractiveSecurity {
  
  var currentPrice: Double = 0
  var currentResolution: Int = 0
  var timedPrice: Array[Double] = null
  var timedActions: Array[Int] = null
  var timedParticipation: Array[Int] = null
  var open: Boolean = false

  // Events 
  val fu_event_magnitude: Double = 20.0
  val fu_event_time = 500; // time tick in 1 .. resolution
  // Global event queue //(#Tick, Magnitude)
  var event_queue =  List[(Int, Double)]()

  // Security

  override def compute_time_series(
      S0: Double, // start_price
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
  ): Array[Double] = new Array[Double](resolution)

  // Interactive Security
  /** 
   * @param action
   * @param resolution 
   * @return true if the action has been reported false if the agent trying to report is out of sync
   **/
  def report(action: Int, resolution: Int): Boolean = {
    if (resolution == currentResolution) {
      timedActions(currentResolution) = timedActions(currentResolution) + action
      timedParticipation(currentResolution) = timedParticipation(currentResolution) + 1
      println("Current Resolution " + currentResolution + " | Security : Current participation " + timedParticipation(currentResolution))
      true
    } else {
      false
    }
  }

  override def aggregate(): Unit = {
    // Very simple market mechanics -- price moves if demand and supply are imbalanced.
    if (currentResolution == 0) {
      currentPrice = initial_price
    } else {
      currentPrice = (timedPrice(currentResolution - 1) * (1.0 + timedActions(currentResolution) / 200.0))
    }
    
    println("Current Resolution : " + currentResolution + " | Security CurrentPrice = " + currentPrice)
    timedPrice(currentResolution) = currentPrice
  }

  override def getCurrentResolution(): Int = {currentResolution}

  override def getOpen(): Boolean = {open}

  override def getCurrentPrice(): Double = {currentPrice}

  /** Notify the value investor of a fundamental event.
      update the player's valuation of the security.

      @param magnitude : quantifies the impact of the event
    */
  def event(magnitude: Int): Unit = {
    val my_mag: Double = magnitude * (1 + 0.1 * Random.nextGaussian().toInt)

    // set time delay for how many time ticks from now the valuation change
    // is to be performed.
    val when = Random.nextInt(50)
    event_queue = (currentResolution, my_mag) :: event_queue
  }

  def main(): Unit = {
    println("Initializing Security : number of actors "+numberActors)
    timedPrice = new Array[Double](resolution)
    timedPrice(0) = currentPrice
    timedActions = new Array[Int](resolution)
    timedParticipation = new Array[Int](resolution)

    // Signals to the other agents that the market is open
    open = true
    println("Security Ready")

    while (currentResolution < resolution) {
        println("---------------------Resolution "+currentResolution+"------------------------")
        // Ensures synchronization between all agents
        // Wait for the actions of all agents to ensure synchronization and correct price
        while (timedParticipation(currentResolution) != numberActors) {
          waitLabel(Turn, 1)
        }

        aggregate()

        currentResolution = currentResolution + 1
    }

    // Plot the security price after the simulation is done
    util.plot("Security price", "time", "price", resolution, List(timedPrice))
  }
}
