name := "AdventOfCode"

version := "0.1"

val commonSettings = Seq(
  scalaVersion := "2.13.4"
)

lazy val root = (project in file(".")).settings(commonSettings).aggregate(aoc2017, aoc2018, aoc2019, aoc2020)

lazy val aoc2017 = (project in file("2017")).settings(commonSettings)

lazy val aoc2018 = (project in file("2018")).settings(commonSettings)

lazy val aoc2019 = (project in file("2019")).settings(commonSettings)

lazy val aoc2020 = (project in file("2020")).settings(commonSettings).settings(
  libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2")
)
