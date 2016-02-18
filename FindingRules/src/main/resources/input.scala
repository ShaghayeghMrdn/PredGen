
import org.apache.spark.lineage.LineageContext
import org.apache.spark.{SparkContext, SparkConf}


object Input {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    var lineage = true

    conf.setMaster("local[1]")
    conf.setAppName("InoutExample" + lineage)

    val sc = new SparkContext(conf)
    val lc = new LineageContext(sc)

    val text = lc.textFile("/Users/shaghayegh/IdeaProjects/First/insurance.txt", 1)
    val records = text.flatMap(line => line.split("\n")).map(data => {val splitted = data.replaceAll(" ", "").split(';'); (splitted(0), Integer.parseInt(splitted(1)), Integer.parseInt(splitted(2)), splitted(3))})
    // val typedRecords = records.map(item => { var employed = false;
    //                       if (item._4.equals("true")) employed = true;
    //                       (item._1, item._2, item._3, employed)})
    // //intermediate result
    // Miryung wants to know what separates failure inducing data from success inducing data 
    // System.out.println(p1, p1(each item in RDD)
    // System.out.println(p2, p2(each item in RDD)

    val result = records.map(  item => {

      var amount = 0;
      //------------>
      //return types and variables in data structure
      System.out.println(item+" : "+item.getClass.getSimpleName);
      if(item._4 == "true") {
        if(item._2 < 1000) {
          if(item._2 < 500) {
            if(item._3 < 40)
              amount = item._2 * 4;
          }
        } else amount = item._2*7;
      }
      (item, amount)})
    println(records.collect().mkString("\n"))
    println(result.collect().mkString("\n"))

  }
}
