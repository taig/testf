package io.taig.testf

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class AutoTest extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutoTest.Macro.apply
}

private object AutoTest {
  final class Macro(val c: blackbox.Context) {
    def apply(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val (tree, tail) = annottees match {
        case head :: tail => (head.tree, tail)
        case _            => c.abort(c.enclosingPosition, "Invalid @AutoTest usage")
      }

      val result = tree match {
        case q"$mods class $name[..$types] $ctorMods(...$paramss) extends ..$parents { $self => ..$body }" =>
          val (autoTests, remainingBody) = findAutoTests(c)(body)
          q"""
          $mods class $name[..$types] $ctorMods(...$paramss) extends ..${adjustParents(
            c
          )(parents)} { $self =>
            ..$remainingBody

            ${auto(c)(autoTests)}
          }
          """
        case q"$mods object $name extends ..$parents { $self => ..$body }" =>
          val (autoTests, remainingBody) = findAutoTests(c)(body)

          q"""
          $mods object $name extends ..${adjustParents(c)(parents)} { $self =>
            ..$remainingBody

            ${auto(c)(autoTests)}
          }
          """
        case _ => c.abort(c.enclosingPosition, "???")
      }

      c.Expr {
        q"""
        $result

        ..$tail
        """
      }
    }

    def adjustParents(
        c: blackbox.Context
    )(parents: Seq[c.Tree]): Seq[c.Tree] = {
      import c.universe._

      val autoTestDiscoveryType = typeOf[AutoTestDiscovery]
      val anyRefType = typeOf[AnyRef]
      val typedParents = parents.map(tree => c.typecheck(q"??? : $tree"))
      val autoTestDiscoveryParent =
        typedParents.find(_.tpe <:< autoTestDiscoveryType)
      val filteredParents = typedParents.filterNot(_.tpe <:< anyRefType)

      autoTestDiscoveryParent.getOrElse(
        tq"_root_.io.taig.testf.AutoTestDiscovery"
      ) +: filteredParents
    }

    def auto(c: blackbox.Context)(autoTests: Seq[c.Tree]): c.Tree = {
      import c.universe._

      q"""
      override final val auto: _root_.cats.effect.IO[_root_.io.taig.testf.Assertion[_root_.io.taig.testf.Pure]] = {
        import _root_.cats.implicits._

        _root_.scala.List[_root_.cats.effect.IO[_root_.io.taig.testf.Test[_root_.io.taig.testf.Pure, _root_.scala.Unit]]](
          ..$autoTests
        ).parSequence.map(_root_.io.taig.testf.Test.and)
      }
      """
    }

    def findAutoTests(
        c: blackbox.Context
    )(body: Seq[c.Tree]): (Seq[c.Tree], Seq[c.Tree]) = {
      import c.universe._
      val (tests, remainingBody) = body.partition(_.isTerm)
      (
        tests.map(tree => q"$tree.void.interpret[_root_.cats.effect.IO]"),
        remainingBody
      )
    }
  }
}
