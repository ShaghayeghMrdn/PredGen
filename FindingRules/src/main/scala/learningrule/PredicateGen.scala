package learningrule

import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.rdd._

import scalax.file.Path
import java.io._

import scala.sys.process._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe._

import at.loveoneanother.schale._

import learningrule.ConstraintObj._
import learningrule.Rules._


object PredicateGen {

	val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
	import tb._, u._

	val rand = scala.util.Random
	val SRC_PATH = "../CUT/src/main/scala/input.scala"

	def main(args: Array[String]): Unit = {

		val source = scala.io.Source.fromFile(SRC_PATH)
		val lines = try source.mkString finally source.close()
		val controlPredicates = parseScalaCode(lines)

		var attributes: ArrayBuffer[Constraint] = new ArrayBuffer()
		controlPredicates.foreach(c => {val newC = new Constraint(); newC.constructFromTree(c); attributes +=newC})

		println("Extracted Predicates:")
		for(a <- attributes)
			println(a.toString())

		// val examples = ArrayBuffer[String]()
		// val inputData = Source.fromFile("../CUT/data.txt")
		// val examplesIt = try inputData.getLines
		// examplesIt.copyToBuffer(examples)
		// inputData.close()
		val rules = sequentialCovering(attributes/*, examples*/)
		println("Learned Rules:")
		println(rules.toString())
	} 

	def parseScalaCode(t: String): List[Tree] = {
		var controlPredicates = List[Tree]()
		try {
			val tree = tb.parse(t)
			new Traverser() {
				override def traverse(t: Tree) = {
					t match {
						case _if: If =>
						var exists = false
						for(prev <- controlPredicates)
							if(_if.cond.equalsStructure(prev))
								exists = true
						
						if(!exists) controlPredicates ::= _if.cond
						_if.foreach(super.traverse)

						case t => super.traverse(t)
					}
				}
			}.traverse(tree)
			return controlPredicates
		}
		catch {
			case ex: ToolBoxError => throw new BadMatchException("ToolBox Match ErrorR")
		}
	}

	def sequentialCovering(attrs:ArrayBuffer[Constraint]/*, examples:ArrayBuffer[String]*/): LearnedRules = {
		val rules: LearnedRules = new LearnedRules()
		var newRule: Rule = null
		var allConstraints = generateAllConstraints(attrs)
		var i = 0
		// var indices = new ArrayBuffer[Integer]()
		// for(i <- 0 until examples.size)
		// 	indices += i
		
		do {
			// prepareForNext(examples, indices)
			newRule = learnOneRule(allConstraints,/* indices,*/ 2)
			rules.addRule(newRule)
			i += 1
		} while(i < 2)

		return rules
	}

	def initiateSpark(): SparkContext = {
		val conf = new SparkConf()

        conf.setMaster("local[1]")
        conf.setAppName("EvaluatePerformance")

        return new SparkContext(conf)	
	}

	def learnOneRule(allConstraints:ArrayBuffer[Constraint], indices: ArrayBuffer[Integer], k:Int): Rule = {
		val sc = initiateSpark()
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
		  	for(h <- candidateHs){
		  		for(c <- allConstraints){
		  			try{
		  				var newH = h.copy.specialize(c)
		  				if(newCandidateHypotheses.indexOf(newH) == -1)
		  					newCandidateHypotheses += newH
		  			} catch {
						case e: IncorrectHypothesis => {}//println(h.toString()+" with "+c.toString())
					}
				}	
			}

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
			for(i <- 0 until k if i < candidatesPerformance.size)
				candidateHs += candidatesPerformance(i)._1

			println("K best candidates:")
			candidateHs.foreach(ch => println(ch.toString()))
			i = i + 1
			if(i == 1) 
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
		val settings = new Settings(str => println("Error: "+str))
		settings.usejavacp.value = true
		var interpreter = new IMain(settings)
		// new PrintWriter(new File("result.txt")
		//---------------spark--------------
        val text = sc.textFile("input.txt", 1)
        val rdds = text.flatMap(line => line.split("\n")).map(line => {val index = new StringOps(line).lastIndexOf(' '); val splitted = line.splitAt(index); (splitted._1, splitted._2.substring(1).toBoolean)})
		println(rdds.collect().mkString("\n"))
		val broadcastVar = sc.broadcast(interpreter)
        val startOfStr = "val item = "
        val endOfStr = "; "+h
		val joinedData = rdds.watchpoint(cls.function).map(pair => {
										val matched = interpret(interpreter, startOfStr+pair._1+endOfStr)
										(pair._1, pair._2, matched)})
		println(joinedData.collect().mkString("\n"))

		// val e = entropy(outputs)
		// println(h+" : "+e)
		println("-----------------------------")
		return e
	}

	def interpret(interpreter: IMain, code: String): Boolean = {
		val result = interpreter.interpret(code)
		if(result != IR.Success)
			throw new FailedEvaluatingPredicate()
		val eval = evaluate()
		//interpreter.close()
		return eval
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

	def evaluatePredicates(h: Hypothesis, examples:ArrayBuffer[String], indices: ArrayBuffer[Integer]) = {
		val hInterpreter = new HInterpreter()
		val matchedIndices = new ArrayBuffer[Integer]()
		indices.foreach(i => {
			val matched = hInterpreter.interpret("val item = "+examples(i)+"; "+h)	
			})
	}
}