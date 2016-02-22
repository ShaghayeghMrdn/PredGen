package learningrule

import java.io._
import scala.io.Source
import scala.tools.nsc.interpreter._
import scala.tools.nsc.Settings
import scala.collection.mutable.ArrayBuffer
import learningrule.ConstraintObj._


object Rules {

	class IncorrectHypothesis(message: String, cause: Throwable = null) extends RuntimeException(message, cause) {}
	class FailedEvaluatingPredicate(message: String = "", cause: Throwable = null) extends RuntimeException(message, cause) {}

	class Hypothesis {
		var clauses: ArrayBuffer[Constraint] = new ArrayBuffer(); //conjunction

		def copy: Hypothesis = {
			val newH = new Hypothesis()
			newH.clauses = this.clauses.clone()
			return newH
		}

		def specialize(newConstraint: Constraint): Hypothesis = {
			for(c <- clauses){
				if(c.equals(newConstraint))
					throw new IncorrectHypothesis("New constraint is already in the hypothesis!") //Is it same as not maximally specific?

				if(c.isNegationOf(newConstraint))
					throw new IncorrectHypothesis("Inconsistent hypothesis!")

				//call Z3(SAT-solver) in order to check satisfiability of the new hypothesis
			}

			this.addConstraint(newConstraint)
			return this
		}

		def addConstraint(newClause: Constraint): Unit = {
			var inserted = false
			for(i <- 0 until clauses.size if !inserted)
				if(newClause < clauses(i)){
					inserted = true
					clauses.insert(i, newClause)
				}

			if(!inserted)
				clauses += newClause
		}

		override def toString(): String = {
			if(clauses.size == 0) 
				return "true"
			
			var result = clauses(0).toString()
			for(i <- 1 until clauses.size)
				result = result.concat(" && "+ clauses(i).toString())
			
			return result
		}

		override def equals(other: Any): Boolean = other match {
			case that: Hypothesis => this.toString().equals(that.toString())
			case _ => false
		}

		def isEmpty(): Boolean = {
			return (clauses.size == 0)
		}
	}

	class Rule(bestH: Hypothesis, predictedVal: Boolean) {
		val hypothesis: Hypothesis = bestH
		val prediction: Boolean = predictedVal

		override def toString(): String = {
			return "IF ("+hypothesis.toString()+") THEN "+prediction.toString();
		}
	}

	class LearnedRules {
		var rules: ArrayBuffer[Rule] = new ArrayBuffer[Rule](); //disjunction

		def addRule(newRule: Rule): Unit = {
			rules += newRule;
		}

		override def toString(): String = {
			var result = ""
			for(r <- rules)
				result = result.concat(r.toString()+"\n")
			return result
		}
	}

	class HInterpreter() {
		
		def interpret(code: String): Boolean = {
			var interpreter = new IMain(new Settings(str => println("Error: "+str)), new PrintWriter(new File("result.txt")))
			val result = interpreter.interpret(code)
			if(result != IR.Success)
				throw new FailedEvaluatingPredicate()
			val eval = evaluate()
			interpreter.close()
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

	}

}

