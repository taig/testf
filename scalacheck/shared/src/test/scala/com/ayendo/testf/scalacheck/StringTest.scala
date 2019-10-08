package com.ayendo.testf.scalacheck

import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Test[Pure] =
    Test("startsWith") {
      Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
        Test.startsWith(a + b, a)
      }
    }

  val concatenate: Test[Pure] =
    Test("concatenate") {
      Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
        Test("lengthA")(Test.gte((a + b).length, a.length)) &
          Test("lengthB")(Test.gte((a + b).length, b.length))
      }
    }

  val substring: Test[Pure] =
    Test("substring") {
      Test.check3(
        Gen.alphaNumStr,
        Gen.alphaNumStr,
        Gen.alphaNumStr
      ) { (a, b, c) =>
        Test.equal((a + b + c).substring(a.length, a.length + b.length), b)
      }
    }

  override val suite: IO[Test[Pure]] =
    Test("StringTest")(startsWith, concatenate, substring).compile
}
