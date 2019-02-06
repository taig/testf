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

  val typelevel: IO[Test[Unit]] =
    request("https://typelevel.org/").map(Test.equal("typelevel", _, 200))

  val scalaLang: IO[Test[Unit]] =
    request("https://www.scala-lang.org/").map(Test.equal("scala-lang", _, 200))

  val github: IO[Test[Unit]] =
    request("https://github.com/").map(Test.equal("github", _, 200))

  override val suite: IO[Test[Unit]] = NonEmptyList
    .of(typelevel, scalaLang, github)
    .parSequence
    .map(_.reduceLeft(_ |+| _))
}
