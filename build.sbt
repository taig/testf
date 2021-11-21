val Version = new {
  val Fs2 = "3.2.2"
  val PortableScalaReflect = "1.1.1"
  val Scala = "3.1.0"
  val ScalajsTestInterface = "1.7.1"
  val TestInterface = "1.0"
}

ThisBuild / scalaVersion := Version.Scala

val githubWorkflowsGenerate = taskKey[List[File]]("Generate GitHub Actions workflows")
val githubWorkflowsCheck = taskKey[Unit](
  "Checks to see if the current workflow files are equivalent to what would be generated and errors otherwise"
)

Global / githubWorkflowsGenerate := {
  val workflows = (LocalRootProject / baseDirectory).value / ".github" / "workflows"
  val main = workflows / "main.yml"
  val branches = workflows / "branches.yml"
  IO.write(main, GithubActionsGenerator.main)
  IO.write(branches, GithubActionsGenerator.branches)
  List(main, branches)
}

Global / githubWorkflowsCheck := {
  val workflows = (LocalRootProject / baseDirectory).value / ".github" / "workflows"
  val main = workflows / "main.yml"
  val branches = workflows / "branches.yml"
  List((main, GithubActionsGenerator.main), (branches, GithubActionsGenerator.branches)).foreach { case (file, yml) =>
    if (IO.read(file) != yml) sys.error(s"$file is not up to date, try running githubWorkflowsGenerate")
  }
}

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
