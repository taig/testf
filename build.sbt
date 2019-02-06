import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val testf = project
  .in(file("."))
  .settings(noPublishSettings ++ releaseSettings)
  .aggregate(coreJVM, coreJS, scalacheckJVM, scalacheckJS, lawsJVM, lawsJS)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(myMavenRepoPublishSettings)
  .settings(
    libraryDependencies ++=
      "org.portable-scala" %%% "portable-scala-reflect" % "0.1.0" ::
        "org.typelevel" %%% "cats-core" % "1.6.0" ::
        "org.typelevel" %%% "cats-effect" % "1.2.0" ::
        "com.lihaoyi" %%% "sourcecode" % "0.1.5" % "test" ::
        Nil,
    name := "testf-core",
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework")
  )
  .jvmSettings(
    libraryDependencies ++=
      "org.scala-sbt" % "test-interface" % "1.0" ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      "org.scala-js" %% "scalajs-test-interface" % "0.6.26" ::
        Nil
  )

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(myMavenRepoPublishSettings)
  .settings(
    libraryDependencies ++=
      "org.scalacheck" %%% "scalacheck" % "1.14.0" ::
        Nil,
    name := "testf-scalacheck",
    sourceGenerators in Compile += Def.task {
      val pkg = s"${organization.value}.testf.scalacheck"
      val name = "ScalacheckTestBuildersN"
      val file = (sourceManaged in Compile).value / s"$name.scala"
      IO.write(file, ScalacheckGenerator(pkg, name))
      Seq(file)
    }.taskValue,
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework")
  )
  .dependsOn(core)

lazy val scalacheckJVM = scalacheck.jvm

lazy val scalacheckJS = scalacheck.js

lazy val laws = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(myMavenRepoPublishSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-laws" % "1.6.0" ::
        "io.chrisdavenport" %%% "cats-scalacheck" % "0.1.0" % "test" ::
        Nil,
    name := "testf-laws",
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework")
  )
  .dependsOn(scalacheck)

lazy val lawsJVM = laws.jvm

lazy val lawsJS = laws.js
