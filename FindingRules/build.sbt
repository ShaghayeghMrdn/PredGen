name := "SequentialCovering"

version := "1.0"

scalaVersion := "2.10.6"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value, 
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"
    )
//libraryDependencies += "org.apache.spark" %% "spark-core" % "1.5.2"
