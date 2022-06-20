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
    val rand = Random.nextInt(3)
    if (rand == 0) {
      if (!sell_inventory.lines.isEmpty) {
        market.enter_sell_order(currentResolution, sell_inventory.lines.head.toOffer(1, this))
      }
    } else if (rand == 1) {
        //val rand = Random.nextInt(wishList.size)
        /**
        val bought = market.enter_buy_order(
        //wishList(rand), 
        Commodities.Ticket,
        currentResolution, 
        this, 
        1, 
        10000
      ) **/

      wishList.foreach(c => market.enter_buy_order( 
        c,
        currentResolution, 
        this, 
        1, 
        10000
      ))

    } else {
      println(name+": No action taken")
    }
  }

  /** Prints status info (Inventory). */
  def show(sell: Boolean): Unit = { 
    if (sell) {
      println("---------------------------------\n"+ 
      name+"\n"+
      sell_inventory.toString()+
      "\n---------------------------------\n")
    } else {
      println("---------------------------------\n"+ 
      name+"\n"+
      inventory.toString()+
      "\n---------------------------------")
    }
  }

  def summary(): Unit = {
    println("---------------"+name+"------------------\n"+ 
      "Sell inventory"+
      sell_inventory.toString()+
      "Inventory"+
      inventory.toString()+
      balance_sheet().toString()
      //"\n---------------------------------\n"
      )
  }

  /** Actor behavior **/
  def main(): Unit = {
    capital = startingCapital
    inventory = start
    sell_inventory = sells
    summary()

    while(!market.open) {
      waitLabel(Turn, 1)
    } 

    //println("Market ready !")

    while (currentResolution < resolution) {
    //println("Owner : Current time " + currentTime)
    //println("Owner : Start action")
    
      while (currentResolution > market.currentResolution) {
          waitLabel(Turn , 1)
      }
      
      action()
      market.report(currentResolution)
      //println("Owner completed action")

      currentResolution = currentResolution + 1
      waitLabel(Turn, 1)
    }

    summary()
  }
}

final case class BalanceSheet(
    balance: Double,
    assets: Double,
    capital: Double,
    liabilities: Double,
    short_positions: Double,
    total_value_destroyed: Double
) {
  override def toString(): String = {
    "Balance Sheet : \n"+
    "Balance : "+balance+"\n"+
    "Assets : "+assets+"\n"+
    "Capital : "+capital+"\n"+
    "Liabilities : "+liabilities+"\n"+
    "Short positions : "+short_positions+"\n"
  }
}

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
        ("\n-------INVENTORY-------"+
        lines.foldLeft("")((s: String, l2: InventoryLine) => s+"\n"+l2.toString())+
        "\n-----------------------\n")
  }

  def removeLine(line: InventoryLine): Unit = {
    var inv_line: InventoryLine = line
    var updated: InventoryLine = null
    
    val match_lines = lines.filter(l => l.commodity == line.commodity && l.price_unit == line.price_unit)

    if (!match_lines.isEmpty) {
      inv_line = match_lines(0)
      lines -= inv_line
      if (inv_line.quantity > line.quantity) {
        updated = InventoryLine(line.commodity, inv_line.quantity - line.quantity, line.price_unit)
        lines += updated
      }
    }
  
    println("After sold : |  "+line.toString+"   |  ")
    purge()
  }

  def addLine(line: InventoryLine): Unit = {
    var inv_line: InventoryLine = null
    var updated = line

    val match_lines = lines.filter(l => l.commodity == line.commodity && l.price_unit == line.price_unit)
    if (!match_lines.isEmpty) {
      inv_line = match_lines(0)
      lines -= inv_line
      updated = InventoryLine(line.commodity, inv_line.quantity - line.quantity, line.price_unit)
    }
    
    lines += updated
    println("After purchase : |  "+line.toString+"   |  ")
  }

  def purge(): Unit = {
    lines.foreach(l => if (l.quantity <= 0) lines -= l)
  }
}

trait Seller {
    def sell(offer: Offer): Unit 
}

trait Buyer {
  def getInventory(): Inventor
  def getName(): String
}

