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
     
    
     **/
}
