val commonSettings = Seq(
  scalaVersion := "2.12.1",
  libraryDependencies := Seq(
    "org.scalaz" %% "scalaz-core",
    "org.scalaz" %% "scalaz-concurrent"
  ).map(_ % "7.2.8")
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
