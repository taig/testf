package com.ayendo.testf

import java.net.{HttpURLConnection, URL}

import cats.effect.{IO, Sync}
import cats.implicits._
import com.ayendo.testf.implicits._

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

  def typelevel[F[_]: Sync]: F[Test] =
    "typelevel" @@ request[F]("https://typelevel.org/").map(Test.equal(_, 200))

  def scalaLang[F[_]: Sync]: F[Test] =
    "scala" @@ request[F]("https://www.scala-lang.org/").map(Test.equal(_, 200))

  def github[F[_]: Sync]: F[Test] =
    "github" @@ request[F]("https://github.com/").map(Test.equal(_, 200))

  override val suite: List[IO[Test]] =
    List(typelevel[IO], scalaLang[IO], github[IO])
}
