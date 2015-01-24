lazy val root = (project in file(".")).settings(
  name := "jsontr",
  version := "0.1.0",
  scalaVersion := "2.10.4",
  libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "1.7.7",
    "org.slf4j" % "slf4j-simple" % "1.7.7",
    "org.mvel" % "mvel2" % "2.2.2.Final",
    "com.fasterxml.jackson.core" % "jackson-core" % "2.5.0",
    // test
    "net.javacrumbs.json-unit" % "json-unit" % "1.5.1" % "test",
    "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.13" % "test",
    "org.specs2" %% "specs2-core" % "2.4.15" % "test"
  )
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-encoding", "utf8",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)

scalacOptions in Test ++= Seq("-Yrangepos")

// Read here for optional jars and dependencies:
// http://etorreborre.github.io/specs2/guide/org.specs2.guide.Runners.html#Dependencies

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
