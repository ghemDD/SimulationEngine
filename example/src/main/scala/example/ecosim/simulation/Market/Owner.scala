package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import scala.collection.mutable.{Map}

/** Owner can act as Seller and Buyer **/
@lift
class Owner(val name: String, 
val startingCapital: Double, 
val wishList: List[Commodity],
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

  def action(): Unit = {
    //val filtered = offerState.filter(o => wishList.exists(c => c.name == o.commodity.name) && o.price_unit <= capital)
    
    if (Random.nextInt(1) == 0) {
      if (!sell_inventory.lines.isEmpty) {
        market.enter_sell_order(currentResolution, sell_inventory.lines.head.toOffer(1, this))
      }
    } else {
        //val rand = Random.nextInt(wishList.size)
        val bought = market.enter_buy_order(
        //wishList(rand), 
        Commodities.Ticket,
        currentResolution, 
        this, 
        1, 
        10000
      ) 
    }
  }

  /** Prints status info (Inventory). */
  def show(sell: Boolean): Unit = { 
    if (sell) {
      println("-----------------"+ name +" ----------------\n"+ 
      sell_inventory.toString()+
      "------------------------------------------")
    } else {
      println("-----------------"+ name +" ----------------\n"+ 
      inventory.toString()+
      "------------------------------------------")
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

final case class BalanceSheet(
    balance: Double,
    assets: Double,
    capital: Double,
    liabilities: Double,
    short_positions: Double,
    total_value_destroyed: Double
)

final case class InventoryLine(commodity: Commodity, quantity: Int, price_unit: Double) {
    override def toString(): String = {
        "Commodity : "+commodity.getName()+" | "+" quantity : "+quantity.toString()+" | price : "+price_unit.toString()
    }

    def toOffer(quant: Int, seller: Seller): Offer = {
      Offer(commodity, seller, quant, price_unit)
    }
}

final case class Inventor(val lines: ListBuffer[InventoryLine]) {
  var liabilities: Double = 0.0
  var value: Double = 0.0
  
  override def toString(): String = {
        ("\n-------INVENTORY-------\n"+
        lines.foldLeft("")((s: String, l2: InventoryLine) => s+"\n"+l2.toString())+
        "\n-----------------------\n")
  }

  def removeLine(line: InventoryLine): Unit = {
    val inv_line = lines.filter(l => l.commodity == line.commodity && l.price_unit == line.price_unit)(0)
    
    if (inv_line.quantity > line.quantity) {
      val updated = InventoryLine(line.commodity, inv_line.quantity - line.quantity, line.price_unit)
      lines += updated
    }

    lines -= inv_line
    
    println("After sold : |  "+line.toString+"   |  ")
  }

  def addLine(line: InventoryLine): Unit = {
    val inv_line = lines.filter(l => l.commodity == line.commodity && l.price_unit == line.price_unit)(0)
    lines -= inv_line
    val updated = InventoryLine(line.commodity, inv_line.quantity - line.quantity, line.price_unit)
    lines += updated

    println("After purchase : |  "+line.toString+"   |  ")
  }
}

trait Seller {
    def sell(offer: Offer): Unit 
}

trait Buyer {
  def getInventory(): Inventor
  def getName(): String
}

