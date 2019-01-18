package com.ayendo.testf.internal

import cats.effect.Sync
import cats.implicits._

import scala.reflect.runtime.universe

object Reflection {
  def loadModule[F[_]](classLoader: ClassLoader, name: String)(
      implicit F: Sync[F]): F[Any] =
    for {
      mirror <- F.delay(universe.runtimeMirror(classLoader))
      module <- F.delay(mirror.staticModule(name))
      instance <- F.delay(mirror.reflectModule(module).instance)
    } yield instance
}
