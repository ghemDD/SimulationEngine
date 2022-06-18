package generated.example.test

import meta.API._

class generatedEcosim extends org.scalatest.FlatSpec {

    "Compiled MarketSim example" should "run" in {
        val agents = generated.example.InitData(1, List[Double](0.9), 100)

        val c = new SimulationConfig(agents, 100000)
        val containerConfig = c.staticPartition(10)(BoundedLatency)

        val results = StartSimulation[AkkaMessagingLayer.type](c)
    }

    

    /**
     *  "Compiled FunSec example" should "run" in {
            val agents = generated.example.InitData(1, 1, 1, 100.0, 500)

            val c = new SimulationConfig(agents, 100000)
            val containerConfig = c.staticPartition(10)(BoundedLatency)

            val results = StartSimulation[AkkaMessagingLayer.type](c)
        }
    **/

    /**
    "FunSec technical dominant" should "run" in {
        val agents = generated.example.ecosim.InitData(20, 30, 50, 334.0, 100)

        val c = new SimulationConfig(agents, 1000)
        val containerConfig = c.staticPartition(10)(BoundedLatency)

        val results = StartSimulation[AkkaMessagingLayer.type](c)
    }

    "FunSec noisy" should "run" in {
        val agents = generated.example.ecosim.InitData(10, 12, 9, 10.0, 1000)

        val c = new SimulationConfig(agents, 100000)
        val containerConfig = c.staticPartition(10)(BoundedLatency)

        val results = StartSimulation[AkkaMessagingLayer.type](c)
    }
    **/
}
