/*import java.io._
import scala.tools.nsc.interpreter._
import scala.tools.nsc.Settings

var inter = "(D3E3,350,25,false)"
val args = inter.substring(0,inter.length()-1).split(",")

val tupleClass = Class.forName("scala.Tuple4")
def getInstance[T](x: Class[T]): T = { val const = x.getConstructors()(0); return const.newInstance(args:_*).asInstanceOf[T]}


val res = getInstance(tupleClass)

var i = new IMain(new Settings(str => println("Error: "+str)), new PrintWriter(new File("result.txt")))

i.bind("item", "Tuple4[String, Int, Int, Boolean]", res)
//-------------------------------
i.interpret("val item = (\"D1 E1\", 900, 35, true)")
i.interpret("item._3 > 40")
*/

