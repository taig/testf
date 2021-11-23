package io.taig.testf

object dsl:
  def label[F[_]](name: String)(test: Test[F]): Test[F] = Test.Label(name, test)

  def test(name: String)(result: => Result): Test[Pure] = label(name)(Test.Assertion(() => result))

  def testF[F[_]](name: String)(result: F[Result]): Test[F] = label(name)(Test.AssertionF(result))

  def result(name: String, result: Result): Test[Pure] = label(name)(Test.Assertion(() => result))

  def success(name: String): Test[Pure] = result(name, Result.Success)

  def error(name: String, message: String): Test[Pure] = result(name, Result.Error(message))

  def failure(name: String, throwable: Throwable): Test[Pure] = result(name, Result.Failure(throwable))

  object group:
    def apply[F[_]](name: String, concurrency: Int = Int.MaxValue)(tests: Test[F]*): Test[F] =
      label(name)(Test.Group(tests.toList, concurrency))

    def sequential[F[_]](name: String)(tests: Test[F]*): Test[F] =
      apply(name, concurrency = 1)(tests: _*)

    def combine[F[_]](tests: Test[F]*): Test[F] = Test.Group(tests.toList, concurrency = Int.MaxValue)
