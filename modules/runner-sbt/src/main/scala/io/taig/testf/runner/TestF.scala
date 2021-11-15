package io.taig.testf.runner

import sbt.testing.*

import scala.concurrent.ExecutionContext

final class TestF extends Framework:
  override val name: String = "TestF"

  override val fingerprints: Array[Fingerprint] = Array(TestF.ModuleFingerprint)

  override def runner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader): TestFRunner =
    new TestFRunner(args, remoteArgs, loader)

  def slaveRunner(
      args: Array[String],
      remoteArgs: Array[String],
      testClassLoader: ClassLoader,
      send: String => Unit
  ): Runner = runner(args, remoteArgs, testClassLoader)

object TestF:
  object ModuleFingerprint extends SubclassFingerprint:
    override val isModule = true

    override val requireNoArgConstructor = true

    override val superclassName = "io.taig.testf.TestApp"
