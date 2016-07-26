scalaVersion := "2.11.8"

fork in run := true

connectInput in run := true

Revolver.settings

mainClass in reStart := Some("kuhn.Song6")

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "junit" % "junit" % "4.12" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps"
)