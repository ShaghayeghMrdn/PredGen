/**
 * Created by Michael on 2/1/16.
 */

import java.io.{PrintWriter, File}
import java.lang.Exception
import java.util.logging._
import org.apache.spark.{ rdd, SparkConf, SparkContext }
import org.apache.spark.api.java.JavaPairRDD
import org.apache.spark.api.java.JavaRDD
import org.apache.spark.api.java.JavaSparkContext
import org.apache.spark.api.java.function.{ FlatMapFunction, Function2, PairFunction }
import org.apache.spark.rdd.{ PairRDDFunctions, RDD }
import scala.Tuple2
import java.util.{ Scanner, Calendar, StringTokenizer }

import scala.collection.mutable.MutableList
import scala.io.Source
import scala.reflect.ClassTag

//remove if not needed
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks._
import org.apache.spark.lineage.LineageContext
import org.apache.spark.lineage.LineageContext._

import org.apache.spark.SparkContext._
import scala.sys.process._

object Classification {
    private val strModelFile = "initial_centroids"
    private val maxClusters = 16
    private val centroids = new Array[Cluster](maxClusters)
    private val centroids_ref = new Array[Cluster](maxClusters)

    def initializeCentroids(): Int = {
        var numClust = 0
        for (i <- 0 until maxClusters) {
            centroids(i) = new Cluster()
            centroids_ref(i) = new Cluster()
        }
        val modelFile = new File(strModelFile)
        val opnScanner = new Scanner(modelFile)
        while (opnScanner.hasNext) {
            val k = opnScanner.nextInt()
            centroids_ref(k).similarity = opnScanner.nextFloat()
            centroids_ref(k).movie_id = opnScanner.nextLong()
            centroids_ref(k).total = opnScanner.nextShort()
            val reviews = opnScanner.next()
            val revScanner = new Scanner(reviews).useDelimiter(",")
            while (revScanner.hasNext) {
                val singleRv = revScanner.next()
                val index = singleRv.indexOf("_")
                val reviewer = new String(singleRv.substring(0, index))
                val rating = new String(singleRv.substring(index + 1))
                val rv = new Review()
                rv.rater_id = reviewer.toInt
                rv.rating = rating.toInt.toByte
                centroids_ref(k).reviews.add(rv)
            }
        }
        for (pass <- 1 until maxClusters) {
            for (u <- 0 until (maxClusters - pass)) {
                if (centroids_ref(u).movie_id < centroids_ref(u + 1).movie_id) {
                    val temp = new Cluster(centroids_ref(u))
                    centroids_ref(u) = centroids_ref(u + 1)
                    centroids_ref(u + 1) = temp
                }
            }
        }
        for (l <- 0 until maxClusters) {
            if (centroids_ref(l).movie_id != -1) {
                numClust = numClust + 1
            }
        }
        numClust
    }

    private val exhaustive = 0

    def main(args: Array[String]): Unit = {
        try {
            //set up logging
            val lm: LogManager = LogManager.getLogManager
            val logger: Logger = Logger.getLogger(getClass.getName)
            val fh: FileHandler = new FileHandler("myLog")
            fh.setFormatter(new SimpleFormatter)
            lm.addLogger(logger)
            logger.setLevel(Level.INFO)
            logger.addHandler(fh)

            //set up spark configuration
            val sparkConf = new SparkConf().setMaster("local[8]")
            sparkConf.setAppName("Classification_LineageDD")
                .set("spark.executor.memory", "3g")

            //set up lineage
            var lineage = true
            var logFile = "hdfs://scai01.cs.ucla.edu:9000/clash/data/"
            if (args.size < 2) {
                logFile = "test_log"
                lineage = true
            } else {
                lineage = args(0).toBoolean
                logFile += args(1)
                sparkConf.setMaster("spark://SCAI01.CS.UCLA.EDU:7077")
            }

            //set up spark context
            val ctx = new SparkContext(sparkConf)

            //set up lineage context and start capture lineage
            val lc = new LineageContext(ctx)
            lc.setCaptureLineage(lineage)

            //generate truth file (for correctness test only)
            /*
            //Prepare for Hadoop MapReduce
            val clw = new commandLineOperations()
            clw.commandLineWorks()
            //Run Hadoop to have a groundTruth
            Seq("hadoop", "jar", "/Users/Michael/Documents/UCLA Senior/F15/Research-Fall2015/benchmark/examples/Classification.jar", "org.apache.hadoop.examples.Classification", "-m", "3", "-r", "1", "/Users/Michael/IdeaProjects/Classification/file1s", "output").!!
            */
            //generate centroid
            val totalClusters = initializeCentroids()

            //start counting the time for the lineage to finish
            val LineageStartTimestamp = new java.sql.Timestamp(Calendar.getInstance.getTime.getTime)
            val LineageStartTime = System.nanoTime()
            logger.log(Level.INFO, "Record Lineage time starts at " + LineageStartTimestamp)

            //run spark with lineage
            val lines = lc.textFile("ratings", 1)
            val classification_result = lines
                //make sure that all records are in the right format
                .filter(line => {
                    val movieIndex = line.indexOf(":")
                    if (movieIndex > 0) true
                    else false
                })
                //seeded faults (for correctness test only):
                //        .filter(line => {
                //          val movieIndex = line.indexOf(":")
                //          val movieIdStr = line.substring(0, movieIndex)
                //          if (movieIdStr.equals("1")) false
                //          else true
                //        })
                //categorize each movie based on the centroid
                .map(line => {
                    var movieIdStr = new String()
                    var reviewStr = new String()
                    var userIdStr = new String()
                    var reviews = new String()
                    var tok = new String("")

                    var clusterId = 0
                    val n = new Array[Int](maxClusters)
                    val sq_a = new Array[Float](maxClusters)
                    val sq_b = new Array[Float](maxClusters)
                    val numer = new Array[Float](maxClusters)
                    val denom = new Array[Float](maxClusters)

                    var max_similarity = 0.0f
                    var similarity = 0.0f
                    val movie = new Cluster()

                    val movieIndex = line.indexOf(":")
                    for (r <- 0 until maxClusters) {
                        numer(r) = 0.0f
                        denom(r) = 0.0f
                        sq_a(r) = 0.0f
                        sq_b(r) = 0.0f
                        n(r) = 0
                    }
                    //MovieIndex is guaranteed to be larger than 0 due to the first filter operation
                    movieIdStr = line.substring(0, movieIndex)
                    val movieId = movieIdStr.toLong
                    movie.movie_id = movieId
                    reviews = line.substring(movieIndex + 1)
                    val token = new StringTokenizer(reviews, ",")

                    while (token.hasMoreTokens) {
                        tok = token.nextToken()
                        val reviewIndex = tok.indexOf("_")
                        userIdStr = tok.substring(0, reviewIndex)
                        reviewStr = tok.substring(reviewIndex + 1)
                        val userId = userIdStr.toInt
                        val review = reviewStr.toInt
                        for (r <- 0 until totalClusters) {
                            breakable {
                                for (q <- 0 until centroids_ref(r).total) {
                                    val rater = centroids_ref(r).reviews.get(q).rater_id
                                    val rating = centroids_ref(r).reviews.get(q).rating.toInt
                                    if (userId == rater) {
                                        numer(r) += (review * rating).toFloat
                                        sq_a(r) += (review * review).toFloat
                                        sq_b(r) += (rating * rating).toFloat
                                        n(r) += 1
                                        break
                                    }
                                }
                            }
                        }
                    }
                    for (p <- 0 until totalClusters) {
                        denom(p) = ((Math.sqrt(sq_a(p).toDouble)) * (Math.sqrt(sq_b(p).toDouble))).toFloat
                        if (denom(p) > 0) {
                            similarity = numer(p) / denom(p)
                            if (similarity > max_similarity) {
                                max_similarity = similarity
                                clusterId = p
                            }
                        }
                    }
                    (clusterId, movieIdStr)
                })
                //the final result format will be key: clusterId, value:<movie_id1>,<movie_id2>,<movie_id3>,<movie_id4>,...
                .reduceByKey(_ + "," + _)
                //.watchpoint(bucket => bucket._2.split(",").size < 60)
                //this final stage throws an exception
                .map(s => {
                    val bList = s._2.split(",")
                    val bList_size = bList.size
                    if (bList_size < 60) {
                        val value = s._2 + "*"
                        (s._1, value)
                    } else {
                        (s._1, s._2)
                    }
                })

            val output = classification_result.collectWithId()

            //      val temp1 = new PrintWriter(new File("/Users/Michael/Desktop/output"))
            //
            for (o <- output) {
                println(o._1._1 + ": " + o._1._2 + " - " + o._2)
            }
            //
            //      temp1.close()
            lc.setCaptureLineage(false)
            Thread.sleep(1000)

            val pw = new PrintWriter(new File("lineageResult"))

            //find the index of the data that cause exception
            var list = List[Long]()
            for (o <- output) {
                val checkPoint = o._1._2.substring(o._1._2.length - 1)
                if (checkPoint.equals("*")) {
                    list = o._2 :: list
                }
            }

            //print out the resulting list for debugging purposes
            //      for (l <- list) {
            //        println("*************************")
            //        println(l)
            //        println("*************************")
            //      }

            var linRdd = classification_result.getLineage()
            linRdd.collect

            linRdd = linRdd.filter(l => {
                println("***" + l + "***") //debug
                list.contains(l)
            })

            //      linRdd = linRdd.goBack()
            //      linRdd = linRdd.goBack()
            linRdd = linRdd.goBackAll()
            //At this stage, technically lineage has already find all the faulty data set, we record the time
            val lineageEndTime = System.nanoTime()
            val lineageEndTimestamp = new java.sql.Timestamp(Calendar.getInstance.getTime.getTime)
            logger.log(Level.INFO, "Lineage takes " + (lineageEndTime - LineageStartTime) / 1000 + " microseconds")
            logger.log(Level.INFO, "Lineage ends at " + lineageEndTimestamp)

            linRdd.show.collect().foreach(s => {
                pw.append(s.toString)
                pw.append('\n')
            })

            pw.close()
            /*
            val lineageResult = ctx.textFile("/Users/Michael/IdeaProjects/Classification_LineageDD/lineageResult", 1)
            //val lineageResult = ctx.textFile("/Users/Michael/IdeaProjects/Classification/file1s", 1)

            val num = lineageResult.count()
            logger.log(Level.INFO, "Lineage caught " + num + " records to run delta-debugging")


            //Remove output before delta-debugging
            val outputFile = new File("/Users/Michael/IdeaProjects/Classification_LineageDD/output")
            if (outputFile.isDirectory) {
                for (list <- Option(outputFile.listFiles()); child <- list) child.delete()
            }
            outputFile.delete

            val DeltaDebuggingStartTimestamp = new java.sql.Timestamp(Calendar.getInstance.getTime.getTime)
            val DeltaDebuggingStartTime = System.nanoTime()
            logger.log(Level.INFO, "Record DeltaDebugging (unadjusted) time starts at " + DeltaDebuggingStartTimestamp)

            /** **************
                * **********
                */
            //lineageResult.cache()

            if (exhaustive == 1) {
                val delta_debug: DD[String] = new DD[String]
                delta_debug.ddgen(lineageResult, new Test,
                    new Split, maxClusters, totalClusters, centroids_ref, lm, fh)
            } else {
                val delta_debug: DD_NonEx[String] = new DD_NonEx[String]
                delta_debug.ddgen(lineageResult, new Test, new Split, maxClusters, totalClusters, centroids_ref, lm, fh)
            }

            val DeltaDebuggingEndTime = System.nanoTime()
            val DeltaDebuggingEndTimestamp = new java.sql.Timestamp(Calendar.getInstance.getTime.getTime)
            logger.log(Level.INFO, "DeltaDebugging (unadjusted) ends at " + DeltaDebuggingEndTimestamp)
            logger.log(Level.INFO, "DeltaDebugging (unadjusted) takes " + (DeltaDebuggingEndTime - DeltaDebuggingStartTime) / 1000 + " microseconds")

            */
            //To print out the result
            //    for (tuple <- output) {
            //      println(tuple._1 + ": " + tuple._2)
            //    }
            println("Job's DONE!")
            ctx.stop()
        }
    }
}
