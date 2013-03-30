name := "ScalaTypes"

version := "0.1"

scalaVersion := "2.10.1"

resolvers += ScalaToolsSnapshots

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0-M9",
  "com.google.guava" % "guava" % "10.0.1"
)
