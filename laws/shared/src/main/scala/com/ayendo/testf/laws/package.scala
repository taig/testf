package com.ayendo.testf

package object laws {
  implicit def lawsTest(test: Test.type): LawsAssertion.type = LawsAssertion
}
