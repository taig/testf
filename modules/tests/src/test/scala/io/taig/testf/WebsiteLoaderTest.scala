package io.taig.testf

import cats.Functor
import cats.syntax.all.*
import java.net.URL
import io.taig.testf.dsl.*

final class WebsiteLoaderTest[F[_]: Functor](loader: URL => F[String]):
  val suite: Test[F] = group("websites")(
    testF("scala.com") {
      loader(new URL("https://scala-lang.com/")).map { html =>
        Assertion.contains(obtained = html, needle = "The Scala Programming Language")
      }
    }
  )

object WebsiteLoaderTest:
  def apply[F[_]: Functor](loader: URL => F[String]): Test[F] =
    new WebsiteLoaderTest[F](loader).suite
