package io.taig.testf

final case class Spec[F[_]](name: String, test: Test[F, Spec[F]], configuration: Configuration)
