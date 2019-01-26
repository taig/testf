package com.ayendo.testf.scalacheck

import cats.Id
import cats.effect.IO
import com.ayendo.testf._
import org.scalacheck.{Gen, Prop}

object StringTest extends TestF {
  val startsWith: Assert[Id] =
    Test.check("startsWith", Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b) =>
        (a + b) startsWith a
    })

  val concatenate: Assert[Id] =
    Test.check("concatenate", Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b) =>
        (a + b).length >= a.length && (a + b).length >= b.length
    })

  val substring: Assert[Id] = Test.check(
    "substring",
    Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        (a + b + c).substring(a.length, a.length + b.length) == b
    })

  override val suite: List[Assert[IO]] =
    List(startsWith, concatenate, substring)
}
