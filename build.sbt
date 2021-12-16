name := "aoc2021"

version := "0.1"

scalaVersion := "3.1.0"

scalacOptions ++= Seq(
  // I tried enabling explicit-nulls, but it causes problems with Java APIs, since every reference type returned from
  // Java is treated as nullable.
//  "-Yexplicit-nulls"
)

// http://www.scala-graph.org/
// This shows how to use a library built with Scala 2.13 from scala3
libraryDependencies += ("org.scala-graph" %% "graph-core" % "1.13.2").cross(CrossVersion.for3Use2_13)
