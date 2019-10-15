package io.taig.testf.runner

import cats.effect.concurrent.MVar
import cats.effect.{ConcurrentEffect, ContextShift, IO}
import sbt.testing._

import scala.concurrent.ExecutionContext

final class TestF extends Framework {
  override val name: String = "TestF"

  override val fingerprints: Array[Fingerprint] =
    Array(TestF.ModuleFingerprint)

  override def runner(
      args: Array[String],
      remoteArgs: Array[String],
      testClassLoader: ClassLoader
  ): TestFRunner =
    TestFRunner(
      args,
      remoteArgs,
      testClassLoader,
      TestF.lock,
      TestF.contextShift
    )

  def slaveRunner(
      args: Array[String],
      remoteArgs: Array[String],
      testClassLoader: ClassLoader,
      send: String => Unit
  ): Runner =
    runner(args, remoteArgs, testClassLoader)
}

object TestF {
  val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val concurrentEffect: ConcurrentEffect[IO] =
    IO.ioConcurrentEffect(contextShift)

  val lock: MVar[IO, Boolean] =
    MVar.of[IO, Boolean](true)(concurrentEffect).unsafeRunSync()

  object ModuleFingerprint extends SubclassFingerprint {
    override val isModule = true

    override val requireNoArgConstructor = true

    override val superclassName = "io.taig.testf.TestApp"
  }
}
