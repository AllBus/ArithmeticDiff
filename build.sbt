ThisBuild / name := "ArithmeticDiff"

ThisBuild / version := "1.1.0"

ThisBuild / scalaVersion := "3.4.2"

lazy val root = (project in file("."))
	.settings(
		name := "scala3Pro"
	)

libraryDependencies += "org.parboiled" %% "parboiled" % "2.5.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

//unmanagedBase <<= baseDirectory { base => base / "libs" }