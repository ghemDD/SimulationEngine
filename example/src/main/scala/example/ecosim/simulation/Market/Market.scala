package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._
import java.util.ArrayList
import scala.collection.mutable.{Map, ListBuffer}
import scala.swing._

 

class UI extends MainFrame {
  title = "Market Simulation"
  preferredSize = new Dimension(720, 480)
  contents = new Label("Resolution : ")
}

@lift
class Market( 
val start_inventory: ListBuffer[Offer],
val numberActors: Int,
val resolution: Int) extends Actor {
//with MarketSelling {

    //type MarketState = Map[Commodity, ListBuffer[(Seller, Int, Double)]]
    //var state: MarketState = Map[Commodity, ListBuffer[(Seller, Int, Double)]]()
    var offerState: ListBuffer[Offer] = null
    var currentResolution: Int = 0
    var open: Boolean = false

    var timedActions: Array[Int] = null
    var timedParticipation: Array[Int] = null


    var offerLog: List[ListBuffer[Offer]] = null

    /**
     * Not needed
    val sellers: List[Seller] = List[Seller]()
    val owners: List[Owner] = List[Owner]()

    // Add agents to the market 

    def add_seller(seller: Seller): Unit = {
        seller :: sellers
        println("Market: Added Seller")
    }

    def add_owner(owner: Owner): Unit = {
        owner :: owners
        println("Market: Added Owner")
    } **/


    // Synch
    def report(time: Int): Boolean = {
        if (time == currentResolution) {
            timedActions(currentResolution) = timedActions(currentResolution) + 1
            timedParticipation(currentResolution) = timedParticipation(currentResolution) + 1
            println("Current participation " + timedParticipation(currentResolution))
            true
        } else {
            println("DESYNC")
            false
        }
    } 

    def aggregate(): Unit = {
        // TODO: Register the min / average / max price for each Commodity
        println("Resolution completed")
    }


    // Market Selling
    // Greedy match
    def enter_buy_order(commodity: Commodity, 
        time: Int, 
        buyer: Buyer, 
        units: Int, 
        limit_price: Double): Int = {

        if (time == currentResolution) {
            // Lowest price first match 
            val nameFilter = offerState.filter(_.commodity.name == commodity.name)

            val priceFilter = nameFilter
                      .filter(o => o.quantity >= units && limit_price >= (units * o.price_unit))

            val filtered = priceFilter
                      .sortWith(_.price_unit <= _.price_unit)

            /** DEBUG
            if (nameFilter.isEmpty) {
                println("Name filter : no match")
            } 

            if (priceFilter.isEmpty) {
                println("Price filter : no match")
            }
            **/
            
            if (filtered.isEmpty) {
                println(buyer.getName()+": No complete match found")
                0
            } else {
                val bestOffer = filtered(0)
                val matchOffer = Offer(bestOffer.commodity, bestOffer.seller, units, bestOffer.price_unit)

                // Update owner inventory
                println(buyer.getName()+" | Purchase registered : "+ matchOffer.toString+" | TOTAL = "+ bestOffer.price_unit * units +" $")
                buyer.getInventory().addLine(InventoryLine(commodity, units, bestOffer.price_unit))

                // Update seller inventory
                bestOffer.seller.sell(bestOffer)

                // Update market state
                offerState -= bestOffer
                val updated = bestOffer.deductQuantity(units)
                if (bestOffer.quantity > units) {
                    offerState += updated
                }

                units
            } 
        } else {
            0
        }
    }

    def enter_sell_order(time: Int, offer: Offer): Boolean = {
        if (time == currentResolution) {
            offerState += offer
            // Debug
            //offer.seller.sell(offer)
            println(offer.toString)
            true
        } else {
            false
        }
    }

    def show(): Unit = {
        println("\n---------------- MARKET ----------------\n"+
        "------BEGIN MARKET STATE : Resolution "+ currentResolution + " ------\n"+
        offerState.foldLeft("")((s, o) => s + o.toString()+ "\n")+
        "------END MARKET STATE------")
    }


    def main(): Unit = {
        // Initialize inventory map
        //state = start_inventory
        timedActions = new Array[Int](resolution)
        timedParticipation = new Array[Int](resolution)
    
        // Display the starting inventory
        offerState = start_inventory
        show()

        open = true
    
        println("OPENING MARKET")

        val ui = new UI
        //ui.visible = true

        while (currentResolution < resolution) {
            println("-------------- RESOLUTION "+currentResolution+"--------------------")
            //show()


            ui.contents = new Label("Resolution "+currentResolution+"\n"+offerState.toString)

            // Wait for the actions of all agents to ensure synchronization
            while (timedParticipation(currentResolution) != numberActors) {
                waitLabel(Turn, 1)
            }

            aggregate()
            currentResolution = currentResolution + 1
            waitLabel(Turn, 1)
        } 

        // Plot Commodities charts
        println("CLOSING MARKET")    
  }
}

final case class Offer(commodity: Commodity, seller: Seller, quantity: Int, price_unit: Double) {
    override def toString(): String = {
        "Offer : "+commodity.getName()+" | "+" quantity : "+quantity.toString()+" | price : "+price_unit.toString()
    }

    def toInventoryLine(): InventoryLine = {
        InventoryLine(commodity, quantity, price_unit)
    }

    def deductQuantity(units: Int): Offer = {
        Offer(commodity, seller, quantity - units, price_unit)
    }
}

trait Product {
    def getName(): String
}


