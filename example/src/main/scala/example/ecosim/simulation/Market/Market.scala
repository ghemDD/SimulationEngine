package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._
import java.util.ArrayList
import scala.collection.mutable.{Map, ListBuffer}

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
        println("AGGREGATE")
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
            
            if (nameFilter.isEmpty) {
                println("Name filter : no match")
            }

            val priceFilter = nameFilter
                      .filter(o => o.quantity >= units && limit_price >= (units * o.price_unit))

            if (priceFilter.isEmpty) {
                println("Price filter : no match")
            }

            val filtered = priceFilter
                      .sortWith(_.price_unit <= _.price_unit)
            
            if (filtered.isEmpty) {
                println("No complete match found")
                0
            } else {
                val bestOffer = filtered(0)

                // Update owner inventory
                println(buyer.getName()+" | Purchase registered : "+ bestOffer.toString+" | TOTAL = "+ bestOffer.price_unit * units+" $")
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
            println("Offer registered : "+ offer.toString)
            true
        } else {
            false
        }
    }

    def show(): Unit = {
        println("""\n---------------- MARKET ----------------\n
        ------BEGIN MARKET STATE------\n
        Current Market State at resolution  """+currentResolution+"\n"+
        offerState.foldLeft("")((s, o) => s + o.toString()+ "\n")+
        """------END MARKET STATE------
        ----------------------------------------\n""")
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

        while (currentResolution < resolution) {
            println("-------------- BEGIN RESOLUTION "+currentResolution+"--------------------")
            //show()
            
            // Wait for the actions of all agents to ensure synchronization
            while (timedParticipation(currentResolution) != numberActors) {
                waitLabel(Turn, 1)
            }

            aggregate()
            currentResolution = currentResolution + 1
            waitLabel(Turn, 1)
            println("-------------- END RESOLUTION "+currentResolution+"--------------------")
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


