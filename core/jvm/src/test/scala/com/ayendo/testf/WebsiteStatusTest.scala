package com.ayendo.testf

import java.net.{HttpURLConnection, URL}

import cats.implicits._
import cats.effect.IO
import com.ayendo.testf.dsl._

object WebsiteStatusTest extends TestF {
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

  val typelevel: Test[IO] = eval("typelevel") {
    request("https://typelevel.org/").map(equal(200))
  }

  val scalaLang: Test[IO] = eval("scala") {
    request("https://www.scala-lang.org/").map(equal(200))
  }

  val github: Test[IO] = eval("github") {
    request("https://github.com/").map(equal(200))
  }

  override val suite: IO[Test[Pure]] =
    test("WebsiteStatusTest")(typelevel, scalaLang, github).compile
}
