package io.taig.testf.scalacheck

import cats.effect.IO
import cats.implicits._
import io.taig.testf._
import io.taig.testf.dsl._
import org.scalacheck.Gen

object StringTest extends TestF {
  val start: Assertion =
    test("startsWith") {
      check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
        startsWith(a)(a + b)
      }
    }

  val concatenate: Assertion =
    test("concatenate") {
      check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
        test("lengthA")(gte(a.length)((a + b).length)) &
          test("lengthB")(gte(b.length)((a + b).length))
      }
    }

  val substring: Assertion =
    test("substring") {
      check3(
        Gen.alphaNumStr,
        Gen.alphaNumStr,
        Gen.alphaNumStr
      ) { (a, b, c) =>
        equal(b)((a + b + c).substring(a.length, a.length + b.length))
      }
    }

  override val suite: IO[Assertion] =
    test("StringTest")(start, concatenate, substring).compile
}
