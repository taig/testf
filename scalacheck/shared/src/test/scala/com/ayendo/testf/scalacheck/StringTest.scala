package com.ayendo.testf.scalacheck

import com.ayendo.testf._
import org.scalacheck.{Gen, Prop}

object StringTest extends TestF {
  val startsWith: Assert =
    Test.check("startsWith", Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b) =>
        (a + b) startsWith a
    })

  val concatenate: Assert =
    Test.check("concatenate", Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b) =>
        (a + b).length >= a.length && (a + b).length >= b.length
    })

  val substring: Assert = Test.check(
    "substring",
    Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        (a + b + c).substring(a.length, a.length + b.length) == b
    })

  override val suite: List[Assert] = List(startsWith, concatenate, substring)
}
