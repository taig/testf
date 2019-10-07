package com.ayendo.testf

import cats.implicits._
import cats.effect.IO

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class AutoTestF(entrypoint: Boolean = true, label: Boolean = true)
    extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AutoTestFMacro.apply
}

private final class AutoTestFMacro(val c: blackbox.Context) {
  def apply(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val (entrypoint, label) = c.prefix.tree match {
      case q"new ${_}(entrypoint = $entrypoint, label = $label)" =>
        (c.eval[Boolean](c.Expr(entrypoint)), c.eval[Boolean](c.Expr(label)))
      case q"new ${_}(entrypoint = $entrypoint)" =>
        (c.eval[Boolean](c.Expr(entrypoint)), true)
      case q"new ${_}(label = $label)" =>
        (true, c.eval[Boolean](c.Expr(label)))
      case q"new ${_}($arguments)" =>
        if (arguments.nonEmpty)
          c.abort(
            c.enclosingPosition,
            "Invalid arguments supplied to @AutoTestF. Please use named arguments."
          )
        else (true, true)
      case q"new ${_}" => (true, true)
    }

    val tree = annottees match {
      case head :: Nil => head.tree
      case _           => c.abort(c.enclosingPosition, "Only objects allowed")
    }

    val result = tree match {
      case q"$mods object ${name: TermName} extends ..$parents { $self => ..$body }" =>
        val parent =
          if (entrypoint) tq"com.ayendo.testf.TestF" +: parents.tail
          else parents

        q"""
        $mods object $name extends ..$parent { $self =>
          ..$body

          val suite: cats.effect.IO[com.ayendo.testf.Test[com.ayendo.testf.Pure]] = {
            ${filter(c)(body.toList, label)}
              .map(com.ayendo.testf.Test.label(${name.encodedName.toString})(_))
          }
        }
        """
      case _ => c.abort(c.enclosingPosition, "Only object allowed")
    }

    c.Expr(result)
  }

  def filter(
      c: blackbox.Context
  )(body: List[c.Tree], label: Boolean): c.Expr[IO[Test[IO]]] = {
    import c.universe._

    val test = typeOf[Test[IO]].typeSymbol

    val tests = body
      .collect { case field: ValOrDefDef if field.tpt.nonEmpty => field }
      .filter {
        case field: DefDef => field.tparams.isEmpty && field.vparamss.isEmpty
        case _: ValDef     => true
      }
      .mapFilter { field =>
        val tpt = c.typecheck(q"??? : ${field.tpt}").tpe.asInstanceOf[TypeRef]
        if (tpt.typeSymbol == test) {
          val test = if (label) autoLabel(c)(field.name) else q"${field.name}"
          val tq"$name[$f]" = field.tpt
          Some((f, test))
        } else None
      }
      .map { case (f, test) => q"$test.compile" }

    c.Expr {
      q"""cats.instances.list.catsStdInstancesForList
            .sequence(List[cats.effect.IO[com.ayendo.testf.Test[com.ayendo.testf.Pure]]](..$tests))
            .map(com.ayendo.testf.Test.group)"""
    }
  }

  def autoLabel(c: blackbox.Context)(term: c.TermName): c.Tree = {
    import c.universe._
    q"com.ayendo.testf.Test.fallback(${term.decodedName.toString}, $term)"
  }
}
