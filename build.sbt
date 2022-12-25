val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "algorithm-w",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-new-syntax",
      "-source", "future" // allow tuple destructuring in for-comprehensions
    ),

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.github.j-mie6" %% "parsley" % "4.0.1"
    )
  )
