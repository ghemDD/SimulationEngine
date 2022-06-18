package example


/** An order; an entry line in an order book. Does not specify whether it's
  * a buy or sell order, but `OB_Line` can be used for either.
 **
  *
  *@param limit If `limit == None`, it's a market order;
  *             otherwise it's a limit order and `limit` is of the form
  *             `Some(limit_price)`.
  */
case class OB_Line(limit: Double, // Some(price) or None
                   time_inserted: Int,
                   units: Int,
                   trader: Owner) {

  /** For tabular display of an order book. */
  def format: String = {
    "%10.2f %4d %3d ".format(limit, time_inserted, units) + " " +
      trader.toString.takeRight(20)
  }

  def deduct_units(n: Int) =
    OB_Line(limit, time_inserted, units - n, trader)

  def set_limit(price: Double) =
    OB_Line(price, time_inserted, units, trader)
}

/**
class OrderBook(
    val security: Security
) extends /* MarketSelling with MarketBuying with
     */ MarketMatchingUtilities[OB_Line] {

  var bid_orderbook: List[OB_Line] = List[OB_Line]()
  var ask_orderbook: List[OB_Line] = List[OB_Line]()

  /** FIXME: for market orders, it uses the current ask_price;
      if there are not enough units on
      offer at that price, it enters a limit order for the rest.
    */
  def enter_buy_order(o: OB_Line): Unit = {
    val price: Double = o.limit.getOrElse(ask_price.getOrElse(0))

    def action(t: OB_Line, n: Int): Unit = {
      //t.trader.atomic_sell_to(o.trader, security, n, t.limit.get)
    }

    val (left_over, aob) = greedy_match_execute(ask_orderbook,
                                                (_: OB_Line).units,
                                                (_: OB_Line).limit.get <= price,
                                                action,
                                                modf,
                                                o.units)

    ask_orderbook = aob

    if (left_over > 0) {
      implicit object ReverseDoubleOrdering extends Ordering[Double] {
        def compare(x: Double, y: Double): Int = -1 * x.compareTo(y)
      }

      // largest limit price first; to break ties, smallest time first.
      bid_orderbook = (o.deduct_units(o.units - left_over).set_limit(price) ::
        bid_orderbook).sorted(Ordering[(Double, Int)].on[OB_Line](ord))
    }
  }

  private def ord = (line: OB_Line) => (line.limit.get, line.time_inserted)

  private def modf(line: OB_Line, n: Int) = line.deduct_units(n)

  /** cancel if cond is true */
  def cancel_buy_order(cond: OB_Line => Boolean): Unit = {
    bid_orderbook = bid_orderbook.filter(!cond(_))
  }

  /** FIXME: for market orders, it uses the bid_price;
      if there are not enough units
      bid for at that price, it enters a limit order for the rest.
    */
  def enter_sell_order(o: OB_Line): Unit = {
    val price = o.limit.getOrElse(bid_price)

    def action(t: OB_Line, n: Int): Unit = {
      //o.trader.atomic_sell_to(t.trader, security, n, t.limit.get)
    }

    val (left_over, bob) = greedy_match_execute(bid_orderbook,
                                                (_: OB_Line).units,
                                                (_: OB_Line).limit.get >= price,
                                                action,
                                                modf,
                                                o.units)

    bid_orderbook = bob

    // smallest limit price first; to break ties, smallest time first.
    if (left_over > 0)
      ask_orderbook = (o.deduct_units(o.units - left_over).set_limit(price) ::
        ask_orderbook).sorted(Ordering[(Double, Int)].on[OB_Line](ord))
  }

  /** cancel if cond is true */
  def cancel_sell_order(cond: OB_Line => Boolean): Unit = {
    ask_orderbook = ask_orderbook.filter(!cond(_))
  }

  /** Deletes expired orders. */
  def purge(time: Int): Unit = {
    bid_orderbook = bid_orderbook.filter(_.expires match {
      case Some(exp_time) if exp_time < time => false
      case _                                 => true
    })
  }

  /** Income from/cost of closing a position of size units.

      Units may be negative -- a short position is allowed and will have a
      negative value.
    */
  def value_of(units: Int): Option[Double] = {
    val (left_over, p) =
      if (units < 0) ask_price(-units); // need to buy back
      else bid_price(units); // need to sell off

    if (units == 0)
      Some(0.0)
    else if (math.abs(units) == left_over)
      None
    else
      Some(p * units / (math.abs(units) - left_over))
    // Note: units here is important for the sign.
  }

  /** returns (#units unmatched, total price) */
  def bid_price(units: Int): (Int, Double) =
    matching_price(bid_orderbook,
                   (_: OB_Line).units,
                   (_: OB_Line).limit.get,
                   units)

  /** returns (#units unmatched, total price) */
  def ask_price(units: Int): (Int, Double) =
    matching_price(ask_orderbook,
                   (_: OB_Line).units,
                   (_: OB_Line).limit.get,
                   units)

  def show(len: Int = 3): Unit = {
    println("----------------------------------------------------")
    println("     Price Time   # Expires    Trader")
    println("(%9.2f)    (%3d)".format(ask_vwap, ask_vol))
    if (ask_orderbook.length > len) println("       ...")
    for (o <- ask_orderbook.take(len).reverse) println(o.format)
    println(
      "  >                                                < " +
        "spread = " + spread)
    for (o <- bid_orderbook.take(len)) println(o.format)
    if (bid_orderbook.length > len) println("       ...")
    println("(%9.2f)    (%3d)".format(bid_vwap, bid_vol))
    println("----------------------------------------------------")
  }

  def bid_vwap: Double =
    if (bid_orderbook.nonEmpty)
      bid_orderbook.map(x => x.limit.get * x.units).sum / bid_vol
    else 0.0

  def bid_vol: Int = bid_orderbook.map(x => x.units).sum

  def ask_vwap: Double =
    ask_orderbook.map(x => x.limit.get * x.units).sum / ask_vol

  def ask_vol: Int = ask_orderbook.map(x => x.units).sum

  def spread: Double = ask_price.getOrElse(1.0 / 0) - bid_price

  def bid_price: Double =
    if (bid_orderbook.nonEmpty) bid_orderbook.head.limit.get
    else 0

  def ask_price: Option[Double] =
    if (ask_orderbook.nonEmpty) ask_orderbook.head.limit
    else None
}**/
