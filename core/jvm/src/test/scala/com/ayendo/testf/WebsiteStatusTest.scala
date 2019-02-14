package com.ayendo.testf

import java.net.{HttpURLConnection, URL}

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._

object WebsiteStatusTest extends TestF {
  def request(url: String): IO[Int] = IO(new URL(url)).flatMap { url =>
    IO(url.openConnection().asInstanceOf[HttpURLConnection])
      .bracket { connection =>
        IO(connection.getResponseCode)
      } { connection =>
        IO(connection.disconnect())
      }
  }

  val typelevel: Test[IO, Unit] =
    Test
      .liftF("typelevel", request("https://typelevel.org/"))
      .flatMap(Test.equal(_, 200))

  val scalaLang: Test[IO, Unit] =
    Test
      .liftF("scala-lang", request("https://www.scala-lang.org/"))
      .flatMap(Test.equal(_, 200))

  val github: Test[IO, Unit] =
    Test
      .liftF("github", request("https://github.com/"))
      .flatMap(Test.equal(_, 200))

  override val suite: Test[IO, Unit] =
    NonEmptyList.of(typelevel, scalaLang, github).reduceLeft(_ |+| _)
}
