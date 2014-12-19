lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.11.4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "hello",
    libraryDependencies += "net.javacrumbs.json-unit" % "json-unit" % "1.5.1"
  )