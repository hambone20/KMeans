package kmeans

import util.Random
import collection.mutable.ArrayBuffer
import scala.math._
import java.io._

class Clustering (var data: Array[Array[Int]], var numAttributes: Int, var szAttribute: Int = 17, var K: Int = 10){
  var clustering = ArrayBuffer[Cluster]()
  var SSE = 0.0
  var entropy = 0.0
  
  val classes = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  var threshold = 0.02//0.04
  var maxIterations = 1000
  
  // create initial random clustering
  initClusters()
  
  var iterations = clusterize(threshold, data) // perform algorithm
  
  def classifyTestPoint(point: Array[Int]): Int = { // TODO
    var minDist = Double.PositiveInfinity
    var minClass = -1
    for(cluster <- clustering){
      val dist = cluster.getDistance(point)
      if(dist < minDist){
        minDist = dist
        minClass = cluster.dominantClass
      }
    }
    
    minClass
  }
  
  def initClusters() {
    for(i <- 0 until K){
      clustering += new Cluster(numAttributes, szAttribute, i)
    }
  }
  
  def findClosestCluster(arr: Array[Int]) = {
    var minDist = Double.PositiveInfinity
    var min: Object = null 
    for(cluster <- clustering){
      val dist = cluster.getDistance(arr)
      if(dist < minDist){
        minDist = dist
        min = cluster
      }
    }
    min.asInstanceOf[Cluster].addMember((minDist, arr(numAttributes), arr)) // add to cluster
    (minDist, min)
  }
  
  def clusterize(terminationThreshold: Double, data: Array[Array[Int]]) = {
    var numIterations = 0
    var delta = 0.0
    clustering.foreach(_.clearMembers())
    do{
    	numIterations += 1
	    // form K clusters by assigning each point to its closest centroid
	    for(example <- data){
	      findClosestCluster(example)
	    }

    	cullEmptyClusters()
	    
	    // recompute the centroid of each cluster
	    delta = recomputeCentroids()
	    println(s"delta = $delta")
	    // check for termination conditions
  	}while(delta > terminationThreshold && numIterations < maxIterations)
  	numIterations
  }
  
  def cullEmptyClusters() {
    for (empty <- clustering.filter(_.isEmpty())){
      putGreatestOutlier(empty)
      assert(!empty.isEmpty(), {println("centroid still empty")})
    }
  }
  
  def putGreatestOutlier(empty: Cluster) = { // put biggest outlier in empty cluster
    var maxDist = 0.0
    var maxOutlier: (Double, Int, Array[Int]) = null
    var maxCluster: Cluster = null
    for (filled <- clustering.filter(_.members.size > 1)){
      val outlier = filled.getOutlier()
      if(outlier._1 > maxDist){
        maxDist = outlier._1
        maxOutlier = outlier
        maxCluster = filled
      }
    }
    println(s"size of maxCluster before pruning: ${maxCluster.members.size}")
    val removed = maxCluster.removeMember(maxOutlier)
    println(s"size of maxCluster after pruning: ${maxCluster.members.size}")
    assert(removed == maxOutlier, {println("Outlier not removed")})
    empty.addMember(maxOutlier)
    assert(!maxCluster.isEmpty(), {println("cluster empty")})
    assert(empty.members.indexOf(maxOutlier) != -1, {println("member not added to cluster")})
    assert(!empty.isEmpty())
  }
  
  def recomputeCentroids(): Double = { // returns amount point has moved
    var delta = 0.0
    
    for(cluster <- clustering.filter(!_.isEmpty())){
      assert(!cluster.isEmpty(), {println("Cluster is empty in recompute")})
      delta += cluster.centerOnMembers()
    }
    
    delta
  }
  
  def getSSE(): Double = {// SSE = sum^k_j=1 sum_x in C_j d(x, m_j)^2   m-> centroid j
    SSE = 0.0
    for(cluster <- clustering){
        SSE += cluster.computeSSE()
    }
    SSE
  }
  
  def getEntropy(): Double = {// ave entropy
    var entropy = 0.0
    var totalSize = 0.0
    for (cluster <- clustering){
      totalSize += cluster.members.size
    }
    for (cluster <- clustering){
      val share = cluster.members.size / totalSize
      assert(share > 0, {println("share inappropriate")})
      val ent = cluster.computeEntropy()
      assert(ent >= 0, {println("entropy less than 1")})
      entropy += share * ent
    }

    entropy
  }
  
  def writePGMS() {
    // delete current pgms
    for{
      
      files <- Option(new File(".").listFiles)
      file <- files if file.getName.endsWith(".pgm")
    } file.delete()
    
    for (cluster <- clustering){
	  val p = new PrintWriter(s"cluster${cluster.clusterNum}-cl${cluster.getDominantClass()}.pgm")
	  p.append(cluster.getPGM())
	  p.close
	}
  }
}