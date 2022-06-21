package example

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import breeze.linalg._
import breeze.plot._
import java.util.ArrayList
import scala.collection.mutable.{Map, ListBuffer}


/**
 * Residual inventory : asynchronous source of the market
 **/
class Stock(start_quantity: Int, commodity: Commodity) 
extends Seller 
with Product {
    var left: Int = start_quantity
    override def sell(offer: Offer): Unit = {
        if (left >= offer.quantity) {
            left = left - offer.quantity
            println("Stock | Sold : "+ commodity.name+" | Quantity : "+offer.quantity+" | Leftover : "+left)
        }
    }

    override def getName(): String = {
        commodity.getName()
    }
}