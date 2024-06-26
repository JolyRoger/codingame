name := "codingame"

version := "0.1"

scalaVersion := "2.13.13"

javacOptions ++= Seq("-source", "21", "-target", "21")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % Test

libraryDependencies += "org.junit.jupiter" % "junit-jupiter-api" % "5.7.0" % Test
