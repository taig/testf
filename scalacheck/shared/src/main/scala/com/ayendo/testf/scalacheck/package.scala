package com.ayendo.testf

package object scalacheck {
  implicit def scalacheckTest(test: Test.type): ScalacheckTest.type =
    ScalacheckTest
}
