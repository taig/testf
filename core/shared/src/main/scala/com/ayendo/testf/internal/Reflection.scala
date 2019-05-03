package com.ayendo.testf.internal

import cats.effect.Sync
import cats.implicits._
import org.portablescala.reflect.{LoadableModuleClass, Reflect}

object Reflection {
  def loadModule[F[_]](classLoader: ClassLoader, name: String)(
      implicit F: Sync[F]
  ): F[Any] =
    for {
      module <- Reflect
        .lookupLoadableModuleClass(name + "$", classLoader)
        .fold[F[LoadableModuleClass]](
          F.raiseError(new ClassNotFoundException(name))
        )(_.pure[F])
      instance <- F.delay(module.loadModule())
    } yield instance
}
