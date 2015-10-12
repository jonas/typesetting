organization in ThisBuild := "io.github.jonas"
name in ThisBuild := "typesetting"
scalaVersion := "2.11.7"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" % "test"
libraryDependencies += "com.typesafe" % "config" % "1.2.1" % "provided"
scalacOptions in Test += "-Yrangepos"
