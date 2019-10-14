import sbtcrossproject.CrossPlugin.autoImport.crossProject

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
    scalacheck.jvm,
    scalacheck.js
  )

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.portable-scala" %%% "portable-scala-reflect" % "0.1.0" ::
        "org.typelevel" %%% "cats-effect" % "2.0.0" ::
        Nil,
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
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

lazy val auto = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(sonatypePublishSettings)
  .settings(
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
  )
  .jsSettings(
    libraryDependencies ++=
      "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
        Nil
  )
  .dependsOn(core)

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
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
    }.taskValue,
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
  )
  .dependsOn(core)

lazy val laws = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(sonatypePublishSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-laws" % "2.0.0" ::
        Nil,
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
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
    )(Resolver.ivyStylePatterns),
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
  )
  .dependsOn(core.jvm)
