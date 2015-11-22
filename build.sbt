scalaVersion := "2.11.7"

val scalazVersion = "7.1.5"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.0-M4",
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "com.gilt" %% "scala-srv-dns" % "0.0.5",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

scalacOptions ++= Seq("-feature", "-deprecation")

initialCommands in console := "import scalaz._, Scalaz._, scalaz.Free._"
