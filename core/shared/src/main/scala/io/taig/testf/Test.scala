package io.taig.testf

import cats._
import cats.effect.IO
import cats.implicits._

sealed abstract class Test[+F[_], +A] extends Product with Serializable {
  def covary[G[α] >: F[α]]: Test[G, A] = this
}

object Test extends Builders {
  final case class And[F[_], A](tests: List[Test[F, A]]) extends Test[F, A]

  final case class Eval[F[_], A](test: F[Test[F, A]]) extends Test[F, A]

  final case class Error(message: String) extends Test[Pure, Nothing]

  final case class Failure(throwable: Throwable) extends Test[Pure, Nothing]

  final case class Label[F[_], A](description: String, test: Test[F, A])
      extends Test[F, A]

  final case class Message[F[_], A](description: String, test: Test[F, A])
      extends Test[F, A]

  final case class Not[F[_], A](test: Test[F, A]) extends Test[F, A]

  final case class Or[F[_], A](tests: List[Test[F, A]]) extends Test[F, A]

  final case class Skip[F[_], A](test: Test[F, A]) extends Test[F, A]

  final case class Success[A](value: A) extends Test[Pure, A]

  final implicit class TestOps[F[_], A](val test: Test[F, A]) extends AnyVal {
    def interpret(implicit interpreter: Interpreter[F]): IO[Test[Pure, A]] =
      interpreter.interpret(test)

    def parInterpret(
        implicit interpreter: ParInterpreter[F]
    ): IO[Test[Pure, A]] = interpreter.interpret(test)

    def seqInterpret(
        implicit interpreter: SeqInterpreter[F]
    ): IO[Test[Pure, A]] = interpreter.interpret(test)

    def mapK[G[α] >: F[α]](f: F ~> G)(implicit F: Functor[F]): Test[G, A] =
      test match {
        case And(tests)                 => And(tests.map(_.mapK(f)))
        case Eval(test)                 => Eval(test.map(_.mapK(f)))
        case test: Error                => test
        case test: Failure              => test
        case Label(description, test)   => Label(description, test.mapK(f))
        case Message(description, test) => Message(description, test.mapK(f))
        case Not(test)                  => Not(test.mapK(f))
        case Or(tests)                  => Or(tests.map(_.mapK(f)))
        case Skip(test)                 => Skip(test.mapK(f))
        case test: Success[A]           => test
      }

    def and(test: Test[F, A]): Test[F, A] = (this.test, test) match {
      case (And(x), And(y)) => And(x ++ y)
      case (And(x), y)      => And(x :+ y)
      case (x, And(y))      => And(x +: y)
      case (x, y)           => And(List(x, y))
    }

    def &(test: Test[F, A]): Test[F, A] = and(test)

    def or(test: Test[F, A]): Test[F, A] = (this.test, test) match {
      case (Or(x), Or(y)) => Or(x ++ y)
      case (Or(x), y)     => Or(x :+ y)
      case (x, Or(y))     => Or(x +: y)
      case (x, y)         => Or(List(x, y))
    }

    def |(test: Test[F, A]): Test[F, A] = or(test)

    def assert(f: A => Assertion[F])(implicit F: Functor[F]): Assertion[F] =
      test.flatMap(f)
  }

  implicit def monad[F[_]: Functor]: Monad[Test[F, *]] =
    new Monad[Test[F, *]] {
      override def pure[A](x: A): Test[F, A] = Success(x)

      override def flatMap[A, B](
          fa: Test[F, A]
      )(f: A => Test[F, B]): Test[F, B] =
        fa match {
          case test: And[F, A]  => And(test.tests.map(flatMap(_)(f)))
          case test: Eval[F, A] => Eval(test.test.map(flatMap(_)(f)))
          case test: Error      => test
          case test: Failure    => test
          case test: Label[F, A] =>
            Label(test.description, flatMap(test.test)(f))
          case test: Message[F, A] =>
            Message(test.description, flatMap(test.test)(f))
          case test: Not[F, A]  => Not(flatMap(test)(f))
          case test: Or[F, A]   => Or(test.tests.map(flatMap(_)(f)))
          case test: Skip[F, A] => Skip(flatMap(test.test)(f))
          case test: Success[A] => f(test.value)
        }

      override def tailRecM[A, B](a: A)(
          f: A => Test[F, Either[A, B]]
      ): Test[F, B] = {
        // TODO tailrec, see https://github.com/typelevel/cats/pull/1041/files#diff-e4d8b82ab5544972195d955591ffe18cR31
        def go(test: Test[F, Either[A, B]]): Test[F, B] =
          test match {
            case test: And[F, Either[A, B]]  => And(test.tests.map(go))
            case test: Eval[F, Either[A, B]] => Eval(test.test.map(go))
            case test: Error                 => test
            case test: Failure               => test
            case test: Label[F, Either[A, B]] =>
              Label(test.description, go(test.test))
            case test: Message[F, Either[A, B]] =>
              Message(test.description, go(test.test))
            case test: Not[F, Either[A, B]]  => Not(go(test.test))
            case test: Or[F, Either[A, B]]   => Or(test.tests.map(go))
            case test: Skip[F, Either[A, B]] => Skip(go(test.test))
            case test: Success[Either[A, B]] =>
              test.value match {
                case Left(a)  => go(f(a))
                case Right(b) => Success(b)
              }
          }

        go(f(a))
      }
    }

  implicit val monadPure: Monad[Test[Pure, *]] = new Monad[Test[Pure, *]] {
    override def pure[A](x: A): Test[Pure, A] = Test.pure(x)

    override def flatMap[A, B](
        fa: Test[Pure, A]
    )(f: A => Test[Pure, B]): Test[Pure, B] =
      fa match {
        case test: And[Pure, A]  => And(test.tests.map(flatMap(_)(f)))
        case test: Eval[Pure, A] => flatMap(test.test)(f)
        case test: Error         => test
        case test: Failure       => test
        case test: Label[Pure, A] =>
          Label(test.description, flatMap(test.test)(f))
        case test: Message[Pure, A] =>
          Message(test.description, flatMap(test.test)(f))
        case test: Not[Pure, A]  => Not(flatMap(test)(f))
        case test: Or[Pure, A]   => Or(test.tests.map(flatMap(_)(f)))
        case test: Skip[Pure, A] => Skip(flatMap(test.test)(f))
        case test: Success[A]    => f(test.value)
      }

    override def tailRecM[A, B](a: A)(
        f: A => Test[Pure, Either[A, B]]
    ): Test[Pure, B] = ???
  }

  implicit def monoidK[F[_]]: MonoidK[Test[F, *]] = new MonoidK[Test[F, *]] {
    override def empty[A]: Test[F, A] = Test.empty

    override def combineK[A](x: Test[F, A], y: Test[F, A]): Test[F, A] = x and y
  }

  implicit def monoid[F[_], A]: Monoid[Test[F, A]] = monoidK[F].algebra

  implicit def eq[A: Eq]: Eq[Test[Pure, A]] = {
    case (And(x), And(y))   => x === y
    case (And(x :: Nil), y) => x === y
    case (x, And(y :: Nil)) => x === y
    case (x: Eval[Pure, A], y: Eval[Pure, A]) =>
      (x.test: Test[Pure, A]) === y.test
    case (Error(x), Error(y)) => x === y
    case (Failure(x), Failure(y)) =>
      x.getMessage === y.getMessage && x.getClass == y.getClass
    case (Label(xd, xt), Label(yd, yt))     => xd === yd && xt === yt
    case (Message(xd, xt), Message(yd, yt)) => xd === yd && xt === yt
    case (Not(x), Not(y))                   => x.test === y.test
    case (Or(x), Or(y))                     => x === y
    case (Or(x :: Nil), y)                  => x === y
    case (x, Or(y :: Nil))                  => x === y
    case (Skip(x), Skip(y))                 => x === y
    case (Success(x), Success(y))           => x === y
    case _                                  => false
  }
}
