import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val catsVersion = "2.1.1"
val catsEffectVersion = "2.1.2"
val hedgehogVersion = "0.1.0"
val portableScalaReflectVersion = "0.1.0"
val scalajsTestInterfaceVersion = "0.6.29"
val scalacheckVersion = "1.14.3"
val testInterfaceVersion = "1.0"

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
  .crossType(CrossType.Pure)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-effect" % catsEffectVersion ::
        "org.portable-scala" %%% "portable-scala-reflect" % portableScalaReflectVersion ::
        Nil
  )

lazy val runnerSbt = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("runner-sbt"))
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      Nil,
    name := "Runner Sbt"
  )
  .jvmSettings(
    libraryDependencies ++=
      "org.scala-sbt" % "test-interface" % testInterfaceVersion ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      "org.scala-js" %% "scalajs-test-interface" % scalajsTestInterfaceVersion ::
        Nil
  )
  .dependsOn(core)

lazy val auto = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(sonatypePublishSettings)
  .jsSettings(
    libraryDependencies ++=
      "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
        Nil
  )
  .dependsOn(core)

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.scalacheck" %%% "scalacheck" % scalacheckVersion ::
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
  .crossType(CrossType.Pure)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-laws" % catsVersion ::
        Nil
  )
  .dependsOn(scalacheck)

lazy val hedgehog = project
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "hedgehog" %% "hedgehog-core" % hedgehogVersion ::
        "hedgehog" %% "hedgehog-runner" % hedgehogVersion ::
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
