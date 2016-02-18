import org.apache.spark.{SparkContext, SparkConf}
import java.io._
import java.lang.StringBuilder

object Input {

    def main(args: Array[String]): Unit = {
        //val fw = new FileWriter("intermediateResults.txt", true)
        val conf = new SparkConf()

        conf.setMaster("local[1]")
        conf.setAppName("InoutExample")

        val sc = new SparkContext(conf)

        val text = sc.textFile("insurance.txt", 1)
        val records = text.flatMap(line => line.split("\n")).map(data => {val splitted = data.split("; "); (splitted(0), Integer.parseInt(splitted(1)), Integer.parseInt(splitted(2)), splitted(3))})
        // val typedRecords = records.map(item => { var employed = false;
        //                       if (item._4.equals("true")) employed = true;
        //                       (item._1, item._2, item._3, employed)})

        val result = records.map(item => {
                                //print(item)
                                // fw.write("item"+" "+item.getClass.getName+" "+item)
                                var amount = 0;
                                if(item._4 == "true") {
                                    if(item._2 < 1000) {
                                        if(item._2 < 500) {
                                            amount = item._2 * 4;
                                        }
                                    } else {
                                        if(item._3 < 40)
                                            amount = item._2*7;
                                        }
                                } else 
                                    if(item._3 > 70)
                                        amount = 1400;
                                println(" "+(amount!=0).toString())
                                (item, amount)})

        //println(records.collect().mkString("\n"))
        println(result.collect().mkString("\n"))
        //fw.close()
    }
}
