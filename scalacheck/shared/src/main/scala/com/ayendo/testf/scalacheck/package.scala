package com.ayendo.testf

package object scalacheck {
  implicit def scalacheckAssertion(test: Test.type): ScalacheckAssertion.type =
    ScalacheckAssertion
}
