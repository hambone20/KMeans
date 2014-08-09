package kmeans
import util.Random
import collection.mutable.ArrayBuffer

class KMeans (data: Array[Array[Int]], var K: Int = 10, var RUNS: Int = 5, var numAttributes: Int = 64, var szAttribute: Int = 17){
	// create K random number seeds
	var clusterings = ArrayBuffer[Clustering]()
	println("clustering")
	for(i <- 0 until RUNS){
	  clusterings += new Clustering(data, numAttributes, szAttribute, K)
	}

	println("finding optimal clustering")
	var optimalClustering = findOptimalClustering
	
	println("writing PGMS")
	optimalClustering.writePGMS()
	println(s"optimal clustering has SSE=${optimalClustering.SSE}")
	println(s"it took ${optimalClustering.iterations} iterations")
	println(s"total entropy = ${optimalClustering.getEntropy()}")
	
	def classifyTest(testData: Array[Array[Int]]) = {// (actual, classied)
	  var results = ArrayBuffer[(Int, Int)]()
	  for(example <- testData){
	    results += Pair(example(numAttributes), optimalClustering.classifyTestPoint(example))
	  }
	  results.toArray
	}

	def findOptimalClustering(): Clustering = { // by least SSE
	  var minSSE = Double.PositiveInfinity
	  var minClustering: Clustering = null
	  for(clustering <- clusterings){
	    val SSE = clustering.getSSE()
	    if(SSE < minSSE){
	      minSSE = SSE
	      minClustering = clustering
	    }
	  }
	  minClustering
	}
}