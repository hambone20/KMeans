package kmeans

import collection.mutable.ArrayBuffer
import util.Random

class Cluster (numAttributes: Int, szAttribute: Int, var clusterNum: Int) {
	var attributes = ArrayBuffer[Double]()
	
	var members = ArrayBuffer[(Double, Int, Array[Int])]() // each member is: distance, class, attributes
	var SSE = 0.0
	var entropy = 0.0
	var dominantClass = -1
	
	val classes = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	
	seedCluster()
	
	def seedCluster() {
	  for (x <- 0 until numAttributes){
	    attributes += Random.nextInt(szAttribute).toDouble
	  }
	}
	
	def computeSSE(): Double = {// SSE = sum^k_j=1 sum_x in C_j d(x, m_j)^2   m-> centroid j
			SSE = 0.0
			for((distance, cl, atts) <- members){
			  SSE += Math.pow(distance, 2.0)
			}
			
			SSE
	}
	
	def computeEntropy(): Double = {// entropy(C_i) = - sum^|classes|_j=1 p_i,j lg p_i,j where p_i,j = probability that a member of cluster i belongs to class j
	  val numInstances = members.size
	  var sum = 0.0
	  for(cl <- classes){
	    // prob cl belongs to this cluster 
	    val numCl = members.filter(_._2 == cl).size
	    val probCl = numCl / numInstances.toDouble
	    assert(probCl >= 0 && probCl <= 1, {println("entropy probability inappropriate")})
	    if(probCl != 0){
	    	val lgp = Math.log(probCl) / Math.log(2.0)
	    	assert(lgp <= 0, {println("log inappropriate")})
	    	sum += probCl * lgp
	    }
	  }
	  sum * -1
	}
	
	def getDominantClass() = {// find majority class
	  var max = 0
	  for(cl <- classes){
	    val sz = members.filter(_._2 == cl).size
	    if(sz > max){
	      max = sz
	      dominantClass = cl
	    }
	  }
	  dominantClass
	}
	
	def centerOnMembers(): Double = {
	  var delta = 0.0
    
      val sz = members.size
      if(sz > 0){
        var newClustering = ArrayBuffer[Array[Double]]()
        var arr = ArrayBuffer[Double]()
        for(idx <- 0 until numAttributes){
          var sum = 0.0
          for(member <- members){
            sum += member._3(idx)
          }
          val mean = sum / sz
          delta += Math.pow(mean - attributes(idx), 2.0)
          arr += mean
        }
        attributes = arr
      } else {
        throw new Exception("empty cluster in centerOnMembers")
      }
	  // TODO check logic here
	  Math.sqrt(delta)
	}
	
	def clearMembers(){
	  members = ArrayBuffer[(Double, Int, Array[Int])]()
	}
	
	def addMember(elem: (Double, Int, Array[Int]))={ // distance, class, attributes
	  members += elem
	}
	
	def isEmpty() = if (members.size < 1) true else false
	
	def getOutlier() = { // find max distance in members and return it
	  members.maxBy(_._1)
	}
	
	def removeMember(elem: (Double, Int, Array[Int])) = {
	  members.remove(members.indexOf(elem))
	}
	
	def getDistance(point: Array[Int]): Double = { // to cluster Note: L2 distance
	   var dist = 0.0
	   for(i <- 0 until numAttributes){
		   dist += Math.pow(point(i) - attributes(i), 2.0)
	   }
	   Math.sqrt(dist)
	}
	
	def getPGM(): String = {
	    var start = 0
	    var end = 64
	    var sz = 8
	    var ret = "P2\n"+
	    s"# dominant class is ${dominantClass}\n" +
	    s"$sz $sz\n" +
	    s"${szAttribute}\n"
	    do{
	    	ret += attributes.slice(start, start+sz).map(_.toInt).mkString(" ") + "\n"
	    	start = start+sz
	    } while (start < end)
	    ret
	}
}