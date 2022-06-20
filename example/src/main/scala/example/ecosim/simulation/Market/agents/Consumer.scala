package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import scala.collection.mutable.{Map}

/** Only consumes the market **/
@lift
class Consumer(val name: String, 
val startingCapital: Double, 
val consumable: Commodity,
val market: Market,
val start: Inventor,
val sells: Inventor,
val resolution: Int) extends Actor 
with Seller
with Buyer {

  // Valuation
  var capital: Double = 0 // $

  // Inventories
  var sell_inventory: Inventor = null
  var inventory: Inventor = null

  // Sync 
  var currentResolution: Int = 0
  var currentTime: Int = 0

  // Not used : parameter taking into account by Bank.scala to deliver credits 
  var probfail: Double = 0.0
  
  // Value 
  def assets(): Double = math.max(0, capital) + inventory.value

  def liabilities(): Double = math.min(0, capital) + inventory.liabilities
    
  def balance_sheet(): BalanceSheet =
    BalanceSheet(
      (assets() + liabilities()).toInt / 100,
      math.max(0.0, capital / 100),
      inventory.value / 100,
      math.min(0.0, capital) / 100,
      inventory.liabilities / 100,
      0
    )

  // Buyer 
  override def getName(): String = {
    name
  }

  override def getInventory(): Inventor = {
    inventory
  }
  
  // Seller
  override def sell(offer: Offer): Unit = {
    // Remove line from sell_inventory
    sell_inventory.removeLine(offer.toInventoryLine())
    capital = capital + offer.price_unit * offer.quantity
    show(true)
  }

  /**
   * Ideally the class Owner would be abstract so we only have to 
   * modify this function to define the behavior of an agent
   **/
  def action(): Unit = {
    //val filtered = offerState.filter(o => wishList.exists(c => c.name == o.commodity.name) && o.price_unit <= capital)
    market.enter_buy_order(
        consumable,
        currentResolution, 
        this, 
        1, 
        capital
      )
  }

  /** Prints status info (Inventory). */
  def show(sell: Boolean): Unit = { 
    if (sell) {
      println("---------------------------------"+ 
      name+"\n"+
      sell_inventory.toString()+
      "---------------------------------")
    } else {
      println("---------------------------------"+ 
      name+"\n"+
      inventory.toString()+
      "---------------------------------")
    }
  }

  /** Actor behavior **/
  def main(): Unit = {
    capital = startingCapital
    inventory = start
    sell_inventory = sells
    show(false)

    while(!market.open) {
      waitLabel(Turn, 1)
    } 

    println("Market ready !")

    while (currentResolution < resolution) {
    //println("Random Investor : Current time " + currentTime)
    println("Random Investor : Start action")
    
      while (currentResolution > market.currentResolution) {
          waitLabel(Turn , 1)
      }
      
      action()
      market.report(currentResolution)
      println("Current Resolution : " + currentResolution + " | Owner action")

      currentResolution = currentResolution + 1
      waitLabel(Turn, 1)
    }
  }
}