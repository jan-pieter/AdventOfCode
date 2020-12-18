name := "AdventOfCode"

version := "0.1"

scalaVersion := "2.13.4"

lazy val aoc2017 = project in file("2017")

lazy val aoc2018 = project in file("2018")

lazy val aoc2019 = project in file("2019")

lazy val aoc2020 = (project in file("2020")).settings(
  libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2")
)
