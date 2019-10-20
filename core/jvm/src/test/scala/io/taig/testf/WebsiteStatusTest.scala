package io.taig.testf

import java.net.{HttpURLConnection, URL}

import cats.implicits._
import cats.effect.IO
import io.taig.testf.dsl._

object WebsiteStatusTest extends TestApp {
  def request(url: String): IO[Int] =
    IO.delay(new URL(url)).flatMap { url =>
      val open = IO.delay(url.openConnection().asInstanceOf[HttpURLConnection])
      val load = { connection: HttpURLConnection =>
        IO.delay(connection.getResponseCode)
      }
      val disconnect = { connection: HttpURLConnection =>
        IO.delay(connection.disconnect())
      }
      open.bracket(load)(disconnect)
    }

  def is200(url: String): Assertion[IO] =
    eval(url)(request(url)).assert(isEqual(200))

  val urls: List[String] = List(
    "https://typelevel.org/",
    "https://www.scala-lang.org/",
    "https://github.com/"
  )

  override val suite: IO[Assertion[Pure]] =
    testAll("WebsiteStatusTest")(urls.map(is200)).interpret
}
