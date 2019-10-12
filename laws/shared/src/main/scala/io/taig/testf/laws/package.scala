package io.taig.testf

package object laws {
  implicit def lawsTest(test: Test.type): LawsAssertion.type = LawsAssertion
}
