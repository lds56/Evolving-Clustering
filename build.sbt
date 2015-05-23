lazy val root = (project in file(".")).
  settings(
    name := "helloworld",
    version := "1.0",
    scalaVersion := "2.11.4",
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4",
      "org.json4s" % "json4s-jackson_2.11" % "3.2.10",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
      "com.github.nscala-time" %% "nscala-time" % "2.0.0"
    )
  )
