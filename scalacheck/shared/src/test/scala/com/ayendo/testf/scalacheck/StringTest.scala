package com.ayendo.testf.scalacheck

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import com.ayendo.testf.implicits._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Test[Id] =
    "startsWith" @@ Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      Test.cond((a + b).startsWith(a))
    }

  val concatenate: Test[Id] =
    "concatenate" @@ Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      "lengthA" @@ Test.cond((a + b).length >= a.length) |+|
        "lengthB" @@ Test.cond((a + b).length >= b.length)
    }

  val substring: Test[Id] =
    "substring" @@ Test.check3(Gen.alphaNumStr,
                               Gen.alphaNumStr,
                               Gen.alphaNumStr) { (a, b, c) =>
      Test.equal((a + b + c).substring(a.length, a.length + b.length), b)
    }

  override val suite: IO[List[Test.Result]] =
    List(startsWith, concatenate, substring).map(_.compile).sequence.pure[IO]
}
