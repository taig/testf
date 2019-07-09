import sbtcrossproject.CrossPlugin.autoImport.crossProject

crossScalaVersions := Seq("2.12.8", scalaVersion.value)

lazy val testf = project
  .in(file("."))
  .settings(noPublishSettings ++ releaseSettings)
  .aggregate(
    autoJVM,
    autoJS,
    coreJVM,
    coreJS,
    hedgehog,
    lawsJVM,
    lawsJS,
    scalacheckJVM,
    scalacheckJS
  )

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(myMavenRepoPublishSettings)
  .settings(
    libraryDependencies ++=
      "org.portable-scala" %%% "portable-scala-reflect" % "0.1.0" ::
        "org.typelevel" %%% "cats-effect" % "2.0.0-M4" ::
        Nil,
    name := "testf-core",
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
      "org.scala-js" %% "scalajs-test-interface" % "0.6.28" ::
        Nil
  )

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val auto = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(myMavenRepoPublishSettings)
  .settings(
    name := "testf-auto",
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
  )
  .dependsOn(core)

lazy val autoJVM = auto.jvm

lazy val autoJS = auto.js

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

lazy val scalacheckJVM = scalacheck.jvm

lazy val scalacheckJS = scalacheck.js

lazy val laws = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .settings(myMavenRepoPublishSettings)
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-laws" % "2.0.0-M4" ::
        Nil,
    name := "testf-laws",
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
  )
  .dependsOn(scalacheck)

lazy val lawsJVM = laws.jvm

lazy val lawsJS = laws.js

lazy val hedgehog = project
  .settings(myMavenRepoPublishSettings)
  .settings(
    libraryDependencies ++=
      "hedgehog" %% "hedgehog-core" % "c36b298d5e61ecaf68f8b607c4578bc65aaaa3f7" ::
        "hedgehog" %% "hedgehog-runner" % "c36b298d5e61ecaf68f8b607c4578bc65aaaa3f7" ::
        Nil,
    name := "testf-hedgehog",
    resolvers += Resolver.url(
      "hedgehog",
      url("https://dl.bintray.com/hedgehogqa/scala-hedgehog")
    )(Resolver.ivyStylePatterns),
    testFrameworks += new TestFramework(
      s"${organization.value}.testf.runner.TestFFramework"
    )
  )
  .dependsOn(coreJVM)
