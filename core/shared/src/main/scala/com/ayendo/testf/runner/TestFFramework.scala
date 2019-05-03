package com.ayendo.testf.runner

import cats.effect.concurrent.MVar
import cats.effect.{ConcurrentEffect, ContextShift, IO}
import sbt.testing._

import scala.concurrent.ExecutionContext

final class TestFFramework extends Framework {
  override val name: String = "TestF"

  override val fingerprints: Array[Fingerprint] =
    Array(TestFFramework.ModuleFingerprint)

  override def runner(
      args: Array[String],
      remoteArgs: Array[String],
      testClassLoader: ClassLoader
  ): TestFRunner =
    TestFRunner(
      args,
      remoteArgs,
      testClassLoader,
      TestFFramework.lock,
      TestFFramework.contextShift
    )

  def slaveRunner(
      args: Array[String],
      remoteArgs: Array[String],
      testClassLoader: ClassLoader,
      send: String => Unit
  ): Runner =
    runner(args, remoteArgs, testClassLoader)
}

object TestFFramework {
  val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val concurrent: ConcurrentEffect[IO] = IO.ioConcurrentEffect(contextShift)

  val lock: MVar[IO, Boolean] =
    MVar.of[IO, Boolean](true)(concurrent).unsafeRunSync()

  object ModuleFingerprint extends SubclassFingerprint {
    override val isModule = true

    override val requireNoArgConstructor: Boolean = true

    override val superclassName: String = "com.ayendo.testf.TestF"
  }
}
