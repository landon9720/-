name := "ƒ"

organization := "kuhn"

description := "Scala language MIDI sequencer"

scalaVersion := "2.11.8"

//fork in run := true

//fork in reStart := true

//connectInput in run := true

//Revolver.settings

//mainClass := Some("kuhn.UI")

mainClass in (Compile, run) := Some("kuhn.UI")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.googlecode.lanterna" % "lanterna" % "3.0.0-beta3",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "junit" % "junit" % "4.12" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps"
)

import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)
