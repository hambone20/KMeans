// Nick Horner
// HW5 - K-Means

import data.Data
import kmeans._

object KMeansTest extends App{
	var trainFile = "optdigits.train"
	if(this.args.size > 0){ // train
		trainFile = this.args(0).toString
	}
  
	var testFile = "optdigits.test"
	if(this.args.size > 1){ // test
		testFile = this.args(1).toString
	}
	
	var K = 10
	if(this.args.size > 2){
	  K = this.args(2).toInt
	}
	
	lazy val trainSet = Data.getData(trainFile)
	lazy val testSet  = Data.getData(testFile)
	
	val classes = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	
	val kmeans = new KMeans(trainSet, K)
	println("classifying test")
	val result = kmeans.classifyTest(testSet)

	println("Confusion Matrix:")
	classes.foreach{x => print(f"${x}%4d")}
	println
	for(_ <- 0 to 44)print("-")
	println
	for(i <- classes){
	  val actuals = result.filter(x => x._1 == i)
	  for(j <- classes){
	      print(f"${actuals.filter(x => x._2 == j).size}%4d")
	  }
	  println(f"|${i}%3d")
	}
	println("NOTE: actuals form far right column")
	val correct = result.filter(x => x._1 == x._2).size
	println(s"Testing Set: $correct out of ${result.size}, for ${correct.toDouble / result.size} accuracy")
}