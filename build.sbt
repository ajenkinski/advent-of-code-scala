name := "aoc2021"

version := "0.1"

scalaVersion := "3.1.0"

scalacOptions ++= Seq(
  // I tried enabling explicit-nulls, but it causes problems with Java APIs, since every reference type returned from
  // Java is treated as nullable.
//  "-Yexplicit-nulls"
)
