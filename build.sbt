val Version = new {
  val Fs2 = "3.2.2"
  val PortableScalaReflect = "1.1.1"
  val Scala = "3.1.0"
  val ScalajsTestInterface = "1.7.1"
  val TestInterface = "1.0"
}

ThisBuild / scalaVersion := Version.Scala

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(
    name := "testf-name",
    libraryDependencies ++=
      ("org.portable-scala" %%% "portable-scala-reflect" % Version.PortableScalaReflect)
        .cross(CrossVersion.for3Use2_13) ::
        Nil
  )

lazy val io = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/io"))
  .settings(
    name := "testf-io",
    libraryDependencies ++=
      "co.fs2" %%% "fs2-core" % Version.Fs2 ::
        Nil
  )
  .dependsOn(core)

lazy val dsl = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/dsl"))
  .settings(
    name := "testf-dsl"
  )
  .dependsOn(core)

lazy val runnerSbt = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/runner-sbt"))
  .settings(
    name := "testf-runner-sbt"
  )
  .jvmSettings(
    libraryDependencies ++=
      "org.scala-sbt" % "test-interface" % Version.TestInterface ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      ("org.scala-js" %% "scalajs-test-interface" % Version.ScalajsTestInterface).cross(CrossVersion.for3Use2_13) ::
        Nil
  )
  .dependsOn(core)

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/tests"))
  .settings(noPublishSettings)
  .settings(
    name := "testf-tests",
    testFrameworks += new TestFramework("io.taig.testf.runner.TestF")
  )
  .dependsOn(dsl, runnerSbt, io)
