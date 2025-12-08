name := "AdventOfCode"

version := "0.1"

val commonSettings = Seq(
  scalaVersion := "3.7.4",
  scalacOptions += "-deprecation"
)

lazy val root = (project in file(".")).settings(commonSettings).aggregate(aoc2017, aoc2018, aoc2019, aoc2020, aoc2021, aoc2022, aoc2023)

lazy val aoc2017 = (project in file("2017")).settings(commonSettings)

lazy val aoc2018 = (project in file("2018")).settings(commonSettings)

lazy val aoc2019 = (project in file("2019")).settings(commonSettings)

lazy val aoc2020 = (project in file("2020")).settings(commonSettings).settings(
  libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0")
)

lazy val aoc2021 = (project in file("2021")).settings(commonSettings).settings(
  libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")
)

lazy val aoc2022 = (project in file("2022")).settings(commonSettings).settings(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    "org.scala-graph" % "graph-core_2.13" % "1.13.6"
  )
)

lazy val aoc2023 = (project in file("2023")).settings(commonSettings).settings(
  libraryDependencies ++= Seq(
    ("org.scala-graph" % "graph-core" % "2.0.1").cross(CrossVersion.for3Use2_13),
    ("org.scala-graph" % "graph-dot" % "2.0.0").cross(CrossVersion.for3Use2_13)
  )
)

lazy val aoc2024 = (project in file("2024")).settings(commonSettings).settings(
  libraryDependencies ++= Seq(
    ("org.scala-graph" % "graph-core" % "2.0.1").cross(CrossVersion.for3Use2_13),
    ("org.scala-graph" % "graph-dot" % "2.0.0").cross(CrossVersion.for3Use2_13)
  )
)

lazy val aoc2025 = (project in file("2025")).settings(commonSettings).settings(
  libraryDependencies ++= Seq(
    ("org.scala-graph" % "graph-core" % "2.0.1").cross(CrossVersion.for3Use2_13),
    ("org.scala-graph" % "graph-dot" % "2.0.0").cross(CrossVersion.for3Use2_13)
  )
)