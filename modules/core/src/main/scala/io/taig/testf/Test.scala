package io.taig.testf

enum Test[+F[_]]:
  case AssertionF(result: F[Result])
  case Assertion(result: () => Result)
  case Group(tests: List[Test[F]], concurrency: Int)
  case Label(name: String, test: Test[F])
  case Skip(test: Test[F])
