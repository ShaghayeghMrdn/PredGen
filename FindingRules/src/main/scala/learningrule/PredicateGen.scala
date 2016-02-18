package learningrule

import org.apache.spark.lineage.LineageContext
import org.apache.spark.{SparkContext, SparkConf}

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

		val examples = ArrayBuffer[String]()
		val inputData = Source.fromFile("../CUT/data.txt")
		val examplesIt = try inputData.getLines
		examplesIt.copyToBuffer(examples)
		inputData.close()
		val rules = sequentialCovering(attributes, examples)
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

	def sequentialCovering(attrs:ArrayBuffer[Constraint], examples:ArrayBuffer[String]): LearnedRules = {
		val rules: LearnedRules = new LearnedRules()
		var newRule: Rule = null
		var allConstraints = generateAllConstraints(attrs)

		var indices = new ArrayBuffer[Integer]()
		for(i <- 0 until examples.size)
			indices += i
		
		do {
			prepareForNext(examples, indices)
			newRule = learnOneRule(allConstraints, indices, 2)
			rules.addRule(newRule)
		} while(indices.size != 0)

		return rules
	}

	def learnOneRule(allConstraints:ArrayBuffer[Constraint], indices: ArrayBuffer[Integer], k:Int): Rule = {
		var candidateHs: ArrayBuffer[Hypothesis] = new ArrayBuffer()
		var bestHypothesis: Hypothesis = new Hypothesis()
		var performanceMap = Map[Hypothesis, Double]()
		var i = 0
		performanceMap(bestHypothesis) = performance(bestHypothesis, indices) //h = true
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
				val newPerformance = performance(newH, indices)
				candidatesPerformance += new Tuple2(newH, newPerformance)
				performanceMap(newH) = newPerformance
			}

			candidatesPerformance = candidatesPerformance.sortWith((A, B) => A._2 > B._2) //Descending sort

			if(candidatesPerformance.size > 0 && candidatesPerformance(0)._2 > performanceMap(bestHypothesis))
				bestHypothesis = candidatesPerformance(0)._1

			println("best hypothesis: "+bestHypothesis.toString())

			candidateHs.clear()
			for(i <- 0 until k if i < candidatesPerformance.size)
				candidateHs += candidatesPerformance(i)._1

			println("K best candidates:")
			candidateHs.foreach(ch => println(ch.toString()))
			i = i + 1
			if(i == 3) 
				candidateHs.clear() //!!!!!!!!!!!!!!!!!!
	
		}

		val (matchedIndices, outputs) = runInput(bestHypothesis, indices, 23)
		println(matchedIndices.mkString(" "))
		matchedIndices.foreach(m => indices -= m)
		println("updated indices: "+indices.mkString(" "))
		var perdiction = true
		var countTrue = 0
		outputs.foreach(o => if(o) countTrue += 1)
		if(countTrue < outputs.size/2)
			perdiction = false
		return new Rule(bestHypothesis, perdiction)
	}

	def generateAllConstraints(attributes: ArrayBuffer[Constraint]): ArrayBuffer[Constraint] = {
		var result = attributes.clone()
		attributes.foreach(c => result += c.negated)
		return result
	}

	def performance(h: Hypothesis, indices: ArrayBuffer[Integer]): Double = {
		val lineNumber = 23
		val (matchedIndices, outputs) = runInput(h, indices, lineNumber)
		val e = entropy(outputs)
		println(h+" : "+e)
		println("-----------------------------")
		return e
		//return rand.nextDouble
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

	def prepareForNext(examples: ArrayBuffer[String], indices: ArrayBuffer[Integer]) {
		val writer = new PrintWriter("../temp/data", "UTF-8")
		indices.foreach(i => writer.println(examples(i)))
		writer.close()
	}

	def evaluatePredicates(h: Hypothesis, examples:ArrayBuffer[String], indices: ArrayBuffer[Integer]) = {
		val hInterpreter = new HInterpreter()
		val matchedIndices = new ArrayBuffer[Integer]()
		indices.foreach(i => {
			val matched = hInterpreter.interpret("val item = "+examples(i)+"; "+h)	
			})
		
	}

	def runInput(h: Hypothesis, indices: ArrayBuffer[Integer], lineNumber: Integer): Tuple2[ArrayBuffer[Integer], ArrayBuffer[Boolean]] = {
		val dstStr = "../temp/src/main/scala/input.scala"
		val src: Path = Path.fromString(SRC_PATH)
		val dst: Path = Path.fromString(dstStr)
		src.copyTo(target=dst, copyAttributes=false, replaceExisting=true)

		val line = "print(\"\\n>>>>>>>\"+("+h.toString()+").toString());"
		val toBeAdded = new ArrayBuffer[String]()
		toBeAdded += line

		val code = Source.fromFile(dstStr).getLines.toBuffer
		code.insertAll(lineNumber, toBeAdded)
		new PrintWriter(dstStr){write(code.mkString("\n")); close}

		var outputs = new ArrayBuffer[Boolean]()
		var matchedIndices = new ArrayBuffer[Integer]() 

		var outputStream = Shell("cd ../temp/ && sbt run")
		var i = 0
		//println(outputStream)
		val pattern = """>>>>>>>(true|false)-->(true|false).*""".r
		for(line <- outputStream) {
			line match {
				case pattern(matched, success) => { //assuming data order has not changed
					if(matched.toBoolean){
						matchedIndices += indices(i)
						outputs += success.toBoolean
					}
					i += 1
				}
				case _ => ;
			}
		}
		println(h+" --> "+outputs.mkString(" ")+" "+outputs.size)
		return (matchedIndices, outputs)
	}



}