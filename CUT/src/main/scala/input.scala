import org.apache.spark.{SparkContext, SparkConf}
import java.io._
import java.lang.StringBuilder

object Input {

    def main(args: Array[String]): Unit = {
        val conf = new SparkConf()
        conf.setMaster("local[1]")
        conf.setAppName("InoutExample")
        
	val sc = new SparkContext(conf)

        val text = sc.textFile("data.txt", 1)
        val records = text.flatMap(line => line.split("\n")).map(data => {val splitted = data.split("; "); (splitted(0), Integer.parseInt(splitted(1)), Integer.parseInt(splitted(2)), splitted(3))})

        val result = records.map(item => {
                                
				var amount = 0;
                                if(item._4 == "true") {
                                    if(item._2 < 1000) {
                                        if(item._2 < 500) {
                                            if(item._3 < 40)
                                                amount = item._2 * 4;
                                        }
                                    } else amount = item._2*7;
                                }
                                println("-->"+(amount!=0).toString())
                                (item, amount)})

        println(result.collect().mkString("\n"))
    }
}
