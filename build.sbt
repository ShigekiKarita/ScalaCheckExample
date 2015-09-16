name := "fpscala"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "latest.integration" % "test"

// scoverage
instrumentSettings

org.scoverage.coveralls.CoverallsPlugin.coverallsSettings

ScoverageKeys.highlighting := true

testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-u", {
  val dir = System.getenv("CI_REPORTS")
  if(dir == null) "target/reports"
  else dir
})