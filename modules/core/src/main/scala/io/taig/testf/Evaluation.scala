package io.taig.testf

enum Evaluation[+A]:
  case Group(values: List[A])
  case Yield(result: Result)
