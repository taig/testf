import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val CatsVersion = "2.1.1"
val CatsEffectVersion = "2.1.2"
val HedgehogVersion = "0.1.0"
val PortableScalaReflectVersion = "1.0.0"
val ScalajsTestInterfaceVersion = "0.6.29"
val ScalacheckVersion = "1.14.3"
val SilencerVersion = "1.6.0"
val TestInterfaceVersion = "1.0"

Global / libraryDependencies ++=
  compilerPlugin(
    "com.github.ghik" % "silencer-plugin" % SilencerVersion cross CrossVersion.full
  ) ::
    ("com.github.ghik" % "silencer-lib" % SilencerVersion % "provided" cross CrossVersion.full) ::
    Nil

val coverageSettings = Def.settings(
  coverageEnabled := {
    if(crossProjectPlatform.value == JSPlatform) false else coverageEnabled.value
  }
)

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
  .settings(sonatypePublishSettings ++ coverageSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-effect" % CatsEffectVersion ::
        "org.portable-scala" %%% "portable-scala-reflect" % PortableScalaReflectVersion ::
        Nil
  )

lazy val runnerSbt = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("runner-sbt"))
  .settings(sonatypePublishSettings ++ coverageSettings)
  .settings(
    name := "Runner Sbt"
  )
  .jvmSettings(
    libraryDependencies ++=
      "org.scala-sbt" % "test-interface" % TestInterfaceVersion ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      "org.scala-js" %% "scalajs-test-interface" % ScalajsTestInterfaceVersion ::
        Nil
  )
  .dependsOn(core)

lazy val auto = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(sonatypePublishSettings ++ coverageSettings)
  .jsSettings(
    libraryDependencies ++=
      "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
        Nil
  )
  .dependsOn(core)

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(sonatypePublishSettings ++ coverageSettings)
  .settings(
    libraryDependencies ++=
      "org.scalacheck" %%% "scalacheck" % ScalacheckVersion ::
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
  .settings(sonatypePublishSettings ++ coverageSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-laws" % CatsVersion ::
        Nil
  )
  .dependsOn(scalacheck)

lazy val hedgehog = project
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "hedgehog" %% "hedgehog-core" % HedgehogVersion ::
        "hedgehog" %% "hedgehog-runner" % HedgehogVersion ::
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
