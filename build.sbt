lazy val root = (project in file(".")).

settings(
  name := "hyperflow-dsl",
  version := "1.0",
  scalaVersion := "2.11.2"
)

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"