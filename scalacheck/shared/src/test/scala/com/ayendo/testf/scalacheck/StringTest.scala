package com.ayendo.testf.scalacheck

import cats.Id
import cats.effect.IO
import com.ayendo.testf.{Summary, Test, TestF}
import org.scalacheck.{Gen, Prop}

object StringTest extends TestF {
  val startsWith: Test[Id] =
    check(Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      (a + b) startsWith a
    })

  val concatenate: Test[Id] =
    check(Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      (a + b).length >= a.length && (a + b).length >= b.length
    })

  val substring: Test[Id] = check(
    Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        (a + b + c).substring(a.length, a.length + b.length) == b
    })

  override val suite: List[IO[Summary]] = List(
    startsWith.run,
    concatenate.run,
    substring.run
  )
}
