name := "dlmScala"

version := "0.1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  // breeze
  "org.scalanlp" %% "breeze" % "0.13",
  // comment if not using native libraries (i.e. only use java libraries)
  "org.scalanlp" %% "breeze-natives" % "0.13",
  // RScala
  "org.ddahl" %% "rscala" % "1.0.15",
  // scala test
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

