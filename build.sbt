lazy val root = (project in file(".")).settings(
  name := "jsontr",
  version := "0.1.0",
  scalaVersion := "2.10.4",
  libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "1.7.7",
    "org.slf4j" % "slf4j-simple" % "1.7.7",
    "net.javacrumbs.json-unit" % "json-unit" % "1.5.1" % "test",
    "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test",
    "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.13" % "test"
  )
)