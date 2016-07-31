name := "ƒ"

organization := "kuhn"

description := "Scala language MIDI sequencer"

scalaVersion := "2.11.8"

fork in run := true

connectInput in run := true

Revolver.settings

mainClass in reStart := Some("kuhn.Song7")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "junit" % "junit" % "4.12" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps"
)