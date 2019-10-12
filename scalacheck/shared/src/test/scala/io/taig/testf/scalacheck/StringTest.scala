package io.taig.testf.scalacheck

import cats.effect.IO
import cats.implicits._
import io.taig.testf._
import io.taig.testf.dsl._
import org.scalacheck.Gen

object StringTest extends TestF {
  val start: Test[Pure] =
    test("startsWith") {
      Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
        startsWith(a)(a + b)
      }
    }

  val concatenate: Test[Pure] =
    test("concatenate") {
      Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
        test("lengthA")(gte(a.length)((a + b).length)) &
          test("lengthB")(gte(b.length)((a + b).length))
      }
    }

  val substring: Test[Pure] =
    test("substring") {
      Test.check3(
        Gen.alphaNumStr,
        Gen.alphaNumStr,
        Gen.alphaNumStr
      ) { (a, b, c) =>
        equal(b)((a + b + c).substring(a.length, a.length + b.length))
      }
    }

  override val suite: IO[Test[Pure]] =
    test("StringTest")(start, concatenate, substring).compile
}
