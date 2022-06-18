package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._

final case class Commodity(val name: String) extends Security 
with Product {

    override def compute_time_series(
      S0: Double, // start_price
      current_time: Double,
      goal_time: Double,
      resolution: Int = 1
    ): Array[Double] = new Array[Double](resolution)

    override def getName(): String = {
        name
    }
}

object Commodities {

  val Wheat = Commodity("Wheat")
  val Flour = Commodity("Flour")
  val Land = Commodity("Land")
  val Ticket = Commodity("Ticket")
  val Beef = Commodity("Beef")
  val Salmon = Commodity("Salmon")
  val Burger = Commodity("Burger")

  val all_commodities = List(Wheat, Flour, Land, Ticket, Beef, Salmon, Burger)
}
