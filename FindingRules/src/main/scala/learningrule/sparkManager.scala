package learningrule

import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.rdd._
import learningrule.ConstraintObj._
import learningrule.Rules._

object SparkManager {
    
    def initiateSpark(): SparkContext = {
        val conf = new SparkConf()

        conf.setMaster("local[1]")
        conf.setAppName("EvaluatePerformance")

        return new SparkContext(conf)   
    }
}