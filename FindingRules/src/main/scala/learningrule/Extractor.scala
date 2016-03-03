package learningrule

import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe._
import scala.collection.mutable.ArrayBuffer

import learningrule.ConstraintObj.{BadMatchException, Constraint}

object Extractor {

    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
    import tb._, u._

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

    def extractPreds(fileName: String): ArrayBuffer[Constraint] = {
        val source = scala.io.Source.fromFile(fileName)
        val lines = try source.mkString finally source.close()
        val controlPredicates = parseScalaCode(lines)

        var attributes: ArrayBuffer[Constraint] = new ArrayBuffer()
        controlPredicates.foreach(c => {
            val newC = new Constraint();
            newC.constructFromTree(c);
            attributes +=newC
        })

        return attributes
    }

}