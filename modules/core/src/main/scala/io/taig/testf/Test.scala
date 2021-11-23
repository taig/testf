package io.taig.testf

enum Test[+F[_], +A]:
  case Effect(result: F[Result])
  case Group(tests: List[A], concurrent: Boolean)
  case Pure(result: () => Result)
