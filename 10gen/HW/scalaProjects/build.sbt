name := "Test Project"
 
version := "1.0.0"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.mongodb" %% "casbah" % "2.6.1"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/releases/"
