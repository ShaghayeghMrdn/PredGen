package learningrule

import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.rdd._

import scalax.file.Path
import java.io._
import scala.sys.process._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
// import at.loveoneanother.schale._

import learningrule.ConstraintObj._
import learningrule.Rules._


object PredicateGen {

	val SRC_PATH = "../CUT/src/main/scala/input.scala"

	def main(args: Array[String]): Unit = {

		var attributes: ArrayBuffer[Constraint] = Extractor.extractPreds(SRC_PATH)
		println("Extracted Predicates:")
		for(a <- attributes)
			println(a.toString())

		val rules = sequentialCovering(attributes)
		println("Learned Rules:")
		println(rules.toString())
	}

	def sequentialCovering(attrs:ArrayBuffer[Constraint]): LearnedRules = {
		val rules: LearnedRules = new LearnedRules()
		var newRule: Rule = null
		var allConstraints = generateAllConstraints(attrs)
		var i = 0
		
		do {
			newRule = learnOneRule(allConstraints, 2)
			rules.addRule(newRule)
			i += 1
		} while(i < 2)

		return rules
	}

	def learnOneRule(allConstraints:ArrayBuffer[Constraint], k:Int): Rule = {
		val sc = SparkManager.initiateSpark()
        //------------------------------
		var candidateHs: ArrayBuffer[Hypothesis] = new ArrayBuffer()
		var bestHypothesis: Hypothesis = new Hypothesis()
		var performanceMap = Map[Hypothesis, Double]()
		var i = 0
		performanceMap(bestHypothesis) = 0.3 //performance(sc, bestHypothesis) //h = true
		candidateHs += bestHypothesis

		var newCandidateHypotheses: ArrayBuffer[Hypothesis] = new ArrayBuffer()
		while(candidateHs.size != 0) {
			newCandidateHypotheses.clear()
		  	for(h <- candidateHs)
		  		for(c <- allConstraints)
		  			try{
		  				var newH = h.copy.specialize(c)
		  				if(newCandidateHypotheses.indexOf(newH) == -1)
		  					newCandidateHypotheses += newH
		  			} catch {
						case e: IncorrectHypothesis => {}//println(h.toString()+" with "+c.toString())
					}
					
			println(newCandidateHypotheses.size) 

			var candidatesPerformance: ArrayBuffer[Tuple2[Hypothesis, Double]] = new ArrayBuffer()
			for(newH <- newCandidateHypotheses){
				val newPerformance = performance(sc, newH)
				candidatesPerformance += new Tuple2(newH, newPerformance)
				performanceMap(newH) = newPerformance
			}

			candidatesPerformance = candidatesPerformance.sortWith((A, B) => A._2 > B._2) //Descending sort
			var index = 0
			if(candidatesPerformance(0)._1.isEmpty())
				index += 1

			if(candidatesPerformance.size > 0 && candidatesPerformance(index)._2 > performanceMap(bestHypothesis))
				bestHypothesis = candidatesPerformance(index)._1

			println("best hypothesis: "+bestHypothesis.toString())

			candidateHs.clear()
			for(i <- index until k+index if i < candidatesPerformance.size)
				candidateHs += candidatesPerformance(i)._1

			println("K best candidates:")
			candidateHs.foreach(ch => println(ch.toString()))
			i = i + 1
			if(i == 2) 
				candidateHs.clear() //!!!!!!!!!!!!!!!!!!
		}

		println("-------------------------------->"+bestHypothesis)
		sc.stop()

//TODO: calculate prediction
		// val (matchedIndices, outputs) = runInput(bestHypothesis, indices, 17)
		// println(matchedIndices.mkString(" "))
		// matchedIndices.foreach(m => indices -= m)
		// println("updated indices: "+indices.mkString(" "))
		var perdiction = true
		// var countTrue = 0
		// outputs.foreach(o => if(o) countTrue += 1)
		// if(countTrue < outputs.size/2)
		// 	perdiction = false
		return new Rule(bestHypothesis, perdiction)
	}

	def generateAllConstraints(attributes: ArrayBuffer[Constraint]): ArrayBuffer[Constraint] = {
		var result = attributes.clone()
		attributes.foreach(c => result += c.negated)
		return result
	}

	def performance(sc: SparkContext, h: Hypothesis): Double = {
		val lineNumber = 17
		//val (matchedIndices, outputs) = runInput(h, indices, lineNumber)
		// new PrintWriter(new File("result.txt")
		//---------------spark--------------
        val text = sc.textFile("input.txt", 1)
        val rdds = text.flatMap(line => line.split("\n")).map(line => {val index = line.lastIndexOf(' '); val splitted = line.splitAt(index); (splitted._1, splitted._2.substring(1).toBoolean)})
		println(rdds.collect().mkString("\n"))
        val startOfStr = "val item = "
        val endOfStr = "; "+h
		val joinedData = rdds.map(pair => {
										val matched = true
										(pair._1, pair._2, matched)})
		println(joinedData.collect().mkString("\n"))

		// val e = entropy(outputs)
		// println(h+" : "+e)
		println("-----------------------------")
		return 0.1
	}

	def evaluate(): Boolean = {
		val pattern = """item:.*= (.*)\nres(\d+): Boolean = (true|false).*""".r
		val source = Source.fromFile("result.txt")
		val lines = try source.mkString finally source.close()
		//val matchedInputs = ArrayBuffer[String]()
		lines match {
			case pattern(value, id, matched) => {println(value+" "+matched)
												 //if(matched.toBoolean) matchedInputs += value
												 return matched.toBoolean}
			case _ => throw new FailedEvaluatingPredicate("Failed matching the output written in file!")
		}
	}

	def log2(x: Double): Double = {
		val lnOf2 = scala.math.log(2)
		if(x == 0)
			return 0.0
		else return (math.log(x)/lnOf2)
	}

	def entropy(outputs: ArrayBuffer[Boolean]): Double = {
		var pSucc = 0.0
		var pFail = 0.0
		for(o <- outputs)
			if(o == true)
				pSucc = pSucc + 1
			else
				pFail = pFail + 1
		val total = pSucc + pFail

		if(total == 0)
			return -2.0

		pSucc /= total
		pFail /= total

		val entropy = pSucc*log2(pSucc) + pFail*log2(pFail)
		return entropy
	}
}