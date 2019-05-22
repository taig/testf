package com.ayendo.testf

import java.net.{HttpURLConnection, URL}

import cats.effect.{IO, Sync}
import cats.implicits._

object WebsiteStatusTest extends TestF {
  def request[F[_]](url: String)(implicit F: Sync[F]): F[Int] =
    F.delay(new URL(url)).flatMap { url =>
      val open = F.delay(url.openConnection().asInstanceOf[HttpURLConnection])
      val load =
        (connection: HttpURLConnection) => F.delay(connection.getResponseCode)
      val disconnect =
        (connection: HttpURLConnection) => F.delay(connection.disconnect())
      F.bracket(open)(load)(disconnect)
    }

  def typelevel[F[_]: Sync]: Test[F] =
    Test.label(
      "typelevel",
      Test.effect(
        request[F]("https://typelevel.org/").map { code =>
          Test.assert(code == 200, "code != 200")
        }
      )
    )

  def scalaLang[F[_]: Sync]: Test[F] =
    Test.label(
      "scala",
      Test.effect(
        request[F]("https://www.scala-lang.org/").map { code =>
          Test.assert(code == 200, "code != 200")
        }
      )
    )

  def github[F[_]: Sync]: Test[F] =
    Test.label(
      "github",
      Test.effect(
        request[F]("https://github.com/").map { code =>
          Test.assert(code == 200, "code != 200")
        }
      )
    )

  override val suite: IO[Test[Pure]] =
    Compiler[IO].compile(
      Test.label(
        "WebsiteStatusTest",
        Test.of(typelevel[IO], scalaLang[IO], github[IO])
      )
    )
}
