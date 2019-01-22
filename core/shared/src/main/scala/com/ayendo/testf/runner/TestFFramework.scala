package com.ayendo.testf.runner

import cats.effect.{ContextShift, IO}
import sbt.testing._

import scala.concurrent.ExecutionContext

final class TestFFramework extends Framework {
  override val name: String = "TestF"

  override val fingerprints: Array[Fingerprint] =
    Array(TestFFramework.ModuleFingerprint)

  override def runner(args: Array[String],
                      remoteArgs: Array[String],
                      testClassLoader: ClassLoader): TestFRunner =
    TestFRunner(args, remoteArgs, testClassLoader, TestFFramework.Async)

  def slaveRunner(args: Array[String],
                  remoteArgs: Array[String],
                  testClassLoader: ClassLoader,
                  send: String => Unit): Runner =
    runner(args, remoteArgs, testClassLoader)
}

object TestFFramework {
  val Async: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  object ModuleFingerprint extends SubclassFingerprint {
    override val isModule = true

    override val requireNoArgConstructor: Boolean = true

    override val superclassName: String = "com.ayendo.testf.TestF"
  }
}
