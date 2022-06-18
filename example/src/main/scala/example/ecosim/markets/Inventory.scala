package example

// Legacy code
class Inventory {
    var inventory: collection.mutable.Map[Security, Int] = null
    var inventory_avg_cost: collection.mutable.Map[Security, Double] = null
    var value: Double = 0.0
    var total_value_destroyed: Double = 0.0

    def liabilities: Double =
    (for ((item, units) <- inventory if units < 0)
      yield units * inventory_avg_cost(item)).sum 

    /** This method is to be called *before* the inventory is updated. */
    private def recalculate_inv_avg_cost(item: Security,
                                       units_added: Int,
                                       unit_cost: Double): Unit = {
                                         
    if (!inventory.contains(item)) add(item, units_added)
    if (inventory(item) + units_added == 0)
      inventory_avg_cost(item) = 0
    else
      inventory_avg_cost(item) = (total_cost(item) +
        units_added * unit_cost) / (inventory(item) + units_added)

    println(
      "recalculate_inv_avg_cost: " + units_added + " " + unit_cost +
        " " + inventory_avg_cost(item))
    }

    def add(item: Security, unit: Int): Unit = {
        inventory += (item -> 0)
        inventory_avg_cost += (item -> 0)
    }


    def total_cost(item: Security): Double =
        inventory(item) * inventory_avg_cost(item) 

    /** Consumes items, which get removed from the inventory and their
      cost gets added to total_value_destroyed.
    */
    final def destroy(item: Security, units: Int): Double = {
        if (!inventory.contains(item)) add(item, 0)
        val value_destroyed = inventory_avg_cost(item) * units
        total_value_destroyed = total_value_destroyed + value_destroyed
        inventory(item) = inventory(item) - units

        value_destroyed // returns cost of destroyed stuff
    }

    /** The format of each entry in the inventory is
      `item_name -> units@cost`.
    */
    override def toString(): String = {
        inventory
        .map(
            t =>
                t._1 + " -> " + t._2 +
                "@" + (inventory_avg_cost(t._1) / 100).toInt)
        .toString
    }
}