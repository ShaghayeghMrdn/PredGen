package learningrule

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.StringOps
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe._

object ConstraintObj {

    class BadMatchException(message: String, cause: Throwable = null) extends RuntimeException(message, cause) {}

    abstract class Operator {
        def toString: String
        def negation: Operator
    }

    case class LessThan extends Operator {
        override def toString: String = { return "<" }
        def negation: Operator = { return new GreaterThanOrEq() }
    }
    case class GreaterThan() extends Operator {
        override def toString: String = { return ">" }
        def negation: Operator = { return new LessThanOrEq() }
    }
    case class LessThanOrEq() extends Operator {
        override def toString: String = { return "<=" }
        def negation: Operator = { return new GreaterThan() }
    }
    case class GreaterThanOrEq() extends Operator {
        override def toString: String = { return ">=" }
        def negation: Operator = { return new LessThan() }
    }
    case class InequalTo() extends Operator {
        override def toString: String = { return "!=" }
        def negation: Operator = { return new EqualTo() }
    }
    case class EqualTo() extends Operator {
        override def toString: String = { return "==" }
        def negation: Operator = { return new InequalTo() }
    }

    abstract class Operand { def toString: String }
    case class Variable(name: String) extends Operand {
        override def toString: String = { return name }
    }
    case class ConstVal(value: String) extends Operand {
        override def toString: String = { return value }
    }

    abstract class Predicate {
        var first: Operand
        var op: Operator
        var second: Operand
    }

    case class Constraint(op1: Operand, opr: Operator, op2: Operand) extends Predicate {
        //println(showRaw(expr))
        first = op1
        op = opr
        second = op2

        def constructFromTree(expr: Tree): Unit = {
            var tokens = expr match {
                case Apply(Select(arg1, opr), List(arg2)) =>
                    (operandConvert(arg1), opConvert(opr.toString()), operandConvert(arg2))
                case Select(_) => (operandConvert(expr), new EqualTo(), new ConstVal("True"))
                case _ => throw new BadMatchException("Matching the whole expr failed!")
            }
            first = tokens._1;
            op = tokens._2;
            second = tokens._3;
        }

        def operandConvert(node: Tree): Operand =
            node match {
                case Literal(Constant(x)) => ConstVal(x.toString())
                case Ident(y) => Variable(y.toString())
                case Select(Ident(w), z) => Variable(w.toString() + '.' + z.toString())
                case x => println(showRaw(x)); throw new BadMatchException("Matching arg failed!")
            }

        def opConvert(opr: String): Operator = opr match {
            case "$less" => new LessThan()
            case "$less$eq=" => new LessThanOrEq()
            case "$greater" => new GreaterThan()
            case "$greater$eq" => new GreaterThanOrEq()
            case "$bang$eq" => new InequalTo()
            case "$eq$eq" => new EqualTo()
            case "equals" => new EqualTo()
            case _ => throw new BadMatchException("Failed in converting " + opr + " to Operator subclass!")
        }

        def printAttr: Unit = {
            println(first.toString())
            println(second.toString())
        }

        def negated: Constraint = {
            return new Constraint(first, op.negation, second)
        }

        def isNegationOf(that: Constraint): Boolean = {
            return this.first.equals(that.first) && this.second.equals(that.second) && this.op.getClass == that.op.negation.getClass
        }

        override def toString(): String = {
            if (second.toString() == "true") //TODO for all strings
                return first.toString() + op.toString() + "\"" + second.toString() + "\""
            return first.toString() + op.toString() + second.toString()
        }

        def <(that: Constraint): Boolean = {
            return new StringOps(this.toString()) < that.toString()
        }
    }
}