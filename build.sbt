ThisBuild / scalaVersion     := "3.2.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala-3-compile-time-business",
    libraryDependencies ++= Seq(
      "dev.zio"     %% "zio"    % "2.0.9",
      "com.lihaoyi" %% "pprint" % "0.8.1",
      "com.lihaoyi" %% "fansi"  % "0.4.0"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
