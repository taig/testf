package io.taig.testf

trait Builder {
  def and[F[_]](tests: List[Test[F]]): Test[F] = Test.And(tests)

  val empty: Test[Pure] = Test.And(List.empty)

  def eval[F[_]](test: F[Test[F]]): Test[F] = Test.Eval(test)

  def label[F[_]](description: String, test: Test[F]): Test[F] =
    Test.Label(description, test)

  def not[F[_]](test: Test[F]): Test[F] = Test.Not(test)

  def message[F[_]](description: String, test: Test[F]): Test[F] =
    Test.Message(description, test)

  def or[F[_]](tests: List[Test[F]]): Test[F] = Test.Or(tests)

  def skip[F[_]](test: Test[F]): Test[F] = Test.Skip(test)

  val success: Test[Pure] = Test.Success

  def error(message: String): Test[Pure] = Test.Error(message)

  def failure(throwable: Throwable): Test[Pure] = Test.Failure(throwable)

  /**
    * Create a `Test` that succeeds when all of the given `tests` succeed
    */
  def allOf[F[_]](tests: Test[F]*): Test[F] =
    if (tests.isEmpty) empty
    else if (tests.length == 1) tests.head
    else tests.reduceLeft(_ and _)

  def test[F[_]](
      description: String
  )(test: Test[F], tests: Test[F]*): Test[F] =
    if (tests.isEmpty) label(description, test)
    else label(description, allOf(test +: tests: _*))

  /**
    * Lift a `F[Test[F]]` into `Test[F]` with a label
    *
    * This is a convenience function for `label(description, eval(test))`.
    */
  def eval[F[_]](description: String)(test: F[Test[F]]): Test[F] =
    label(description, eval(test))

  /**
    * Apply a label to `test` if it doesn't have one yet
    */
  def fallback[F[_]](description: String)(test: Test[F]): Test[F] =
    test match {
      case label: Test.Label[F] => label
      case test                 => label(description, test)
    }

  /**
    * Create a `Test` that succeeds when at least one of the given `tests`
    * succeeds
    */
  def oneOf[F[_]](tests: Test[F]*): Test[F] =
    if (tests.isEmpty) empty
    else if (tests.length == 1) tests.head
    else tests.reduceLeft(_ or _)

  /**
    * Create a `Test` that always succeeds with a label
    *
    * This is a convenience function for `label(description, success)`.
    */
  def success(description: String): Test[Pure] = label(description, success)
}
