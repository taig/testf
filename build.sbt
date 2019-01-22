lazy val root = project
  .in(file("."))
  .settings(noPublishSettings ++ releaseSettings)
  .aggregate(core)

lazy val core = project
  .settings(myMavenRepoPublishSettings)
  .settings(
    libraryDependencies ++=
      "com.lihaoyi" %% "sourcecode" % "0.1.5" ::
        "org.scala-sbt" % "test-interface" % "1.0" ::
        "org.typelevel" %% "cats-core" % "1.5.0" ::
        "org.typelevel" %% "cats-effect" % "1.2.0" ::
        "org.typelevel" %% "kittens" % "1.2.0" ::
        "com.github.mpilquist" %% "simulacrum" % "0.15.0" % "compile" ::
        "org.typelevel" %% "cats-laws" % "1.5.0" % "test" ::
        Nil,
    name := "testf-core",
    testFrameworks += new TestFramework(
      "com.ayendo.testf.runner.TestFFramework")
  )
