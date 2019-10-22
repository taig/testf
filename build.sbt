import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val testf = project
  .in(file("."))
  .settings(noPublishSettings)
  .aggregate(
    auto.jvm,
    auto.js,
    core.jvm,
    core.js,
    hedgehog,
    laws.jvm,
    laws.js,
    runnerSbt.jvm,
    runnerSbt.js,
    scalacheck.jvm,
    scalacheck.js,
    tests.jvm,
    tests.js
  )

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-effect" % "2.0.0" ::
        "org.portable-scala" %%% "portable-scala-reflect" % "0.1.0" ::
        Nil
  )

lazy val runnerSbt = crossProject(JVMPlatform, JSPlatform)
  .in(file("runner-sbt"))
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      Nil,
    name := "Runner Sbt"
  )
  .jvmSettings(
    libraryDependencies ++=
      "org.scala-sbt" % "test-interface" % "1.0" ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      "org.scala-js" %% "scalajs-test-interface" % "0.6.29" ::
        Nil
  )
  .dependsOn(core)

lazy val auto = crossProject(JVMPlatform, JSPlatform)
  .settings(sonatypePublishSettings)
  .jsSettings(
    libraryDependencies ++=
      "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
        Nil
  )
  .dependsOn(core)

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.scalacheck" %%% "scalacheck" % "1.14.2" ::
        Nil,
    sourceGenerators in Compile += Def.task {
      val pkg = s"${organization.value}.testf"
      val name = "ScalacheckAssertionN"
      val file = (sourceManaged in Compile).value / s"$name.scala"
      IO.write(file, ScalacheckGenerator(pkg, name))
      Seq(file)
    }.taskValue
  )
  .dependsOn(core)

lazy val laws = crossProject(JVMPlatform, JSPlatform)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-laws" % "2.0.0" ::
        Nil
  )
  .dependsOn(scalacheck)

lazy val hedgehog = project
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "hedgehog" %% "hedgehog-core" % "0.1.0" ::
        "hedgehog" %% "hedgehog-runner" % "0.1.0" ::
        Nil,
    resolvers += Resolver.url(
      "hedgehog",
      url("https://dl.bintray.com/hedgehogqa/scala-hedgehog")
    )(Resolver.ivyStylePatterns)
  )
  .dependsOn(core.jvm)

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .settings(noPublishSettings)
  .settings(
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestF"
    )
  )
  .dependsOn(core, runnerSbt, auto, scalacheck, laws)
  .jvmConfigure(_.dependsOn(hedgehog))

addCommandAlias(
  "testJVM",
  ";core/test;auto/test;scalacheck/test;laws/test;hedgehog/test"
)
addCommandAlias(
  "testJS",
  ";coreJS/test;autoJS/test;scalacheckJS/test;lawsJS/test"
)
