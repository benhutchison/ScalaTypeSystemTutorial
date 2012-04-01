name := "ScalaTypes"

version := "0.1"

scalaVersion := "2.9.1"

resolvers += ScalaToolsSnapshots

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT",
  "com.google.guava" % "guava" % "10.0.1"
)
