package example

object FunSec extends App {
    val liftedMain = meta.classLifting.liteLift {
        def apply(randomCount: Int, valueCount: Int, technicalCount: Int, initial_price: Double, resolution: Int): List[Actor] = {
          
          val security = new FundamentalSecurity(initial_price, technicalCount + valueCount + randomCount, resolution)
          // Init list of actors
          val valueInvestors = (1 to valueCount).map(_ => {
              val init_val = initial_price + initial_price * 0.05 * Random.nextGaussian()
              val eagerness = 0.1 + 0.01 * Random.nextGaussian()
              new ValueInvestor(security, init_val, eagerness, resolution)
              }).toList

          val technicalInvestors = (1 to technicalCount).map(_ => {
                              new TechnicalInvestor(security, 1 + Random.nextInt(5),
                              0.1 + Random.nextGaussian() * 0.05,
                              initial_price, resolution)
                            }).toList

          val randomInvestors = (1 to randomCount).map(_ => new RandomInvestor(security, resolution)).toList
          val size = valueInvestors.size + technicalInvestors.size + randomInvestors.size
          

          security :: (valueInvestors ++ technicalInvestors ++ randomInvestors)
        }
    }

    val randomReflected: ClassWithObject[RandomInvestor] = RandomInvestor.reflect(IR)
    val technicalReflected: ClassWithObject[TechnicalInvestor] = TechnicalInvestor.reflect(IR)
    val valueReflected: ClassWithObject[ValueInvestor] = ValueInvestor.reflect(IR)
    val securityReflected: ClassWithObject[FundamentalSecurity] = FundamentalSecurity.reflect(IR)

    compileSims(List(randomReflected, technicalReflected, valueReflected, securityReflected), Some(liftedMain))
}
