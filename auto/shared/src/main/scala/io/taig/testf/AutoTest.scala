package io.taig.testf

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class AutoTest extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macro.apply
}

private final class Macro(val c: blackbox.Context) {
  def apply(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val tree = annottees match {
      case head :: Nil => head.tree
      case _           => c.abort(c.enclosingPosition, "Only objects allowed")
    }

    val appType = typeOf[AutoTestApp]
    val anyRefType = typeOf[AnyRef]

    val result = tree match {
      case q"$mods object $name extends ..$parents { $self => ..$body }" =>
        val filteredParents = parents.filterNot { tree =>
          val parent = c.typecheck(q"??? : $tree").tpe
          parent <:< appType || parent <:< anyRefType
        }

        val autoTestAppParents = tq"_root_.io.taig.testf.AutoTestApp" +: filteredParents

        val (autoTests, remainingBody) = findAutoTests(c)(body)

        q"""
        $mods object $name extends ..$autoTestAppParents { $self =>
          ..$remainingBody

          override final val auto: _root_.cats.effect.IO[_root_.io.taig.testf.Assertion[_root_.io.taig.testf.Pure]] = {
            import _root_.cats.implicits._

            _root_.scala.List[_root_.cats.effect.IO[_root_.io.taig.testf.Test[_root_.io.taig.testf.Pure, _root_.scala.Unit]]](
              ..${autoTests}
            ).parSequence.map(_root_.io.taig.testf.Test.and)
          }
        }
        """
      case _ => c.abort(c.enclosingPosition, "Only object allowed")
    }

    c.Expr(result)
  }

  def findAutoTests(
      c: blackbox.Context
  )(body: Seq[c.Tree]): (Seq[c.Tree], Seq[c.Tree]) = {
    import c.universe._

    val testType = typeOf[Test[Any, Any]]

    val q"{..$typedBody}" = c.typecheck(q"{..$body}")
    val tests = collection.mutable.ListBuffer.empty[c.Tree]
    val remainingBody = collection.mutable.ListBuffer.empty[c.Tree]

    typedBody.zipWithIndex.foreach {
      case (tree, index) =>
        if (tree.tpe <:< testType) tests += q"${body(index)}.void.interpret"
        else remainingBody += body(index)
    }

    (tests.toSeq, remainingBody.toSeq)
  }
}
