import Dependencies._

//ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "pagerank_multiplex"
ThisBuild / organizationName := "pagerank_multiplex"

lazy val root = (project in file("."))
  .settings(
    name := "PageRank Multiplex",
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

// Breeze

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies  ++= Seq(
  // Last stable release
  //"org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze" % "1.0-RC4",
  
  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes. 
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  //"org.scalanlp" %% "breeze-natives" % "0.13.2"
  "org.scalanlp" %% "breeze-natives" % "1.0-RC4"
)

