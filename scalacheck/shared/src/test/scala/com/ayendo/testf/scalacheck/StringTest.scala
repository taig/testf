package com.ayendo.testf.scalacheck

import com.ayendo.testf.{Test, TestF}
import org.scalacheck.{Gen, Prop}

object StringTest extends TestF {
  val startsWith: Test =
    check("startsWith", Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b) =>
        (a + b) startsWith a
    })

  val concatenate: Test =
    check("concatenate", Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b) =>
        (a + b).length >= a.length && (a + b).length >= b.length
    })

  val substring: Test = check(
    "substring",
    Prop.forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        (a + b + c).substring(a.length, a.length + b.length) == b
    })

  override val suite: List[Test] = List(startsWith, concatenate, substring)
}
