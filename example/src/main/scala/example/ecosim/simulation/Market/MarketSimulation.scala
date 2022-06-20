package example

import scala.collection.mutable.{Map, ListBuffer}

object MarketSimulation extends App {
    val liftedMain = meta.classLifting.liteLift {
        def apply(ownerCount: Int, initial_prices: List[Double], resolution: Int): List[Actor] = {

        type Commodity = example.Commodity
        type Seller = example.Seller
        type Offer = example.Offer
        type Inventor = example.Inventor
        type InventoryLine = example.InventoryLine
        type Stock = example.Stock

        val stock: Seller = new Stock(10, example.Commodities.Beef)
        val offer = new Offer(example.Commodities.Beef, stock, 80, 5.25)
        val offer2 = new Offer(example.Commodities.Salmon, stock, 89, 4.54)
        val offer3 = new Offer(example.Commodities.Ticket, stock, 90, 12.99)
        val offer4 = new Offer(example.Commodities.Land, stock, 100, 450.22883)

        val start_inventory = ListBuffer[Offer](offer, offer2, offer3, offer4)
        val wishList_jackson = List(example.Commodities.Ticket, example.Commodities.Beef)
        val wishList_mercury = List(example.Commodities.Land, example.Commodities.Beef)

        val owner_inventory = new Inventor(ListBuffer(example.InventoryLine(example.Commodities.Beef, 8, 3.44)))
        val owner_sell_inventory = new Inventor(ListBuffer(example.InventoryLine(example.Commodities.Ticket, 13, 99.44)))
        val owner_sell_inv_2 = new Inventor(ListBuffer(example.InventoryLine(example.Commodities.Land, 16, 23.2)))

        val market = new Market(
        //List(beef), 
        //Map[Security, (Seller, Int)](), 
        start_inventory,
        2, // numberActors
        //List(beef),
        resolution)

        val jackson = new Owner("Michael Jackson", 
          27.9, 
          wishList_jackson,
          market, 
          owner_inventory, 
          owner_sell_inventory, 
          resolution
        )

        val mercury = new Owner("Freddy Mercury", 
          76.9, 
          wishList_mercury,
          market, 
          owner_sell_inventory, 
          owner_sell_inv_2, 
          resolution
        )

        /**

        val ticketMachine = new Source("UGC TicketMachine", 
          1000, 
          Commodities.Ticket,
          market,
          new Inventor(new ListBuffer[InventoryLine]()),
          new Inventor(new ListBuffer[InventoryLine]()),
          resolution
        )

        val consumer = new Consumer("Beef eater", 
          100000, 
          Commodities.Beef,
          market,
          new Inventor(new ListBuffer[InventoryLine]()),
          new Inventor(new ListBuffer[InventoryLine]()),
          resolution
        )

        val person = new Person(
          "Joao", 
          1000, 
          wishList,
          market,
          owner_inventory,
          owner_sell_inventory,
          resolution
        ) **/
          
        //List(jackson, mercury, ticketMachine, consumer, person)
        List(jackson, mercury, market)
      }
    }

    val ownerReflected: ClassWithObject[Owner] = Owner.reflect(IR)
    val marketReflected: ClassWithObject[Market] = Market.reflect(IR)
    //val sourceReflected: ClassWithObject[Source] = Source.reflect(IR)
    //val consumerReflected: ClassWithObject[Consumer] = Consumer.reflect(IR)
    //val personReflected: ClassWithObject[Person] = Person.reflect(IR)

    compileSims(List(ownerReflected, marketReflected), 
    Some(liftedMain))
}