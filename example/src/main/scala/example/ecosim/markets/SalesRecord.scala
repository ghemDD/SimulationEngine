package example

case class SalesRecord(
      buyer: Owner,
      matching: List[(Owner, Int, Int)], // (Seller, Units, Unit_Price)
      num_ordered: Int,
      num_sold: Int,
      total_price: Int
  ) {
    def missed_sales(at_price: Double): Int = {
      if (total_price > num_sold * at_price)
        num_ordered // we would have made a cheaper offer
      else num_ordered - num_sold
      // Even though we are expensive, we make the deal
      // because it's a market order
    }
  }
