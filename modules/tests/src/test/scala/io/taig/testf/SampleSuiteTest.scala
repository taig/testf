package io.taig.testf

import cats.effect.IO
import io.taig.testf.dsl.*

import java.net.URL
import scala.io.Source

object SampleSuiteTest extends IOTestApp:
  val loader: URL => IO[String] = url => IO(Source.fromURL(url).mkString)

  override val suite: IO[Test[IO]] = IO.pure {
    group.combine(WebsiteLoaderTest(loader))
  }
