import sbt.Tests.{Group, SubProcess}

val scala3Version = "3.8.1"
ThisBuild / organization := "org.bargsten"
ThisBuild / organizationName := "Joachim Bargsten"
ThisBuild / organizationHomepage := Some(url("https://bargsten.org/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/jwbargsten/scala-tstype"),
    "scm:git@github.com:jwbargsten/scala-tstype.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "jwbargsten",
    name = "Joachim Bargsten",
    email = "jw@bargsten.org",
    url = url("https://bargsten.org")
  )
)
ThisBuild / versionScheme := Some("early-semver")
Global / pgpSigningKey := sys.env.get("PGP_KEY_ID")

ThisBuild / description := "generate TypeScript models from Scala"
ThisBuild / licenses := List(
  "Apache 2" -> new URI("http://www.apache.org/licenses-2.0.txt").toURL
)
ThisBuild / homepage := Some(url("https://github.com/jwbargsten/valacc"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-tstype",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-Yexplicit-nulls",
      "-Wsafe-init",
      "-Ycheck-all-patmat",
      "-feature",
      "-source:future",
      "-deprecation",
      "-Wconf:id=E029:e"
    ),
    Test / fork := true,
    Test / parallelExecution := true,
    // Each suite in its own JVM, run suites in parallel.
    Test / testGrouping := (Test / definedTests).value.map { suite => Group(suite.name, Seq(suite), SubProcess(ForkOptions())) },
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.2.4" % Test,
    ),
  )

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

ThisBuild / concurrentRestrictions := Seq(
  Tags.limitAll(java.lang.Runtime.getRuntime.availableProcessors),
  Tags.limit(Tags.ForkedTestGroup, 4)
)
