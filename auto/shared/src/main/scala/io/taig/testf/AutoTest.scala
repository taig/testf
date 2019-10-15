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

          final override val auto: _root_.cats.effect.IO[_root_.io.taig.testf.Assertion[_root_.io.taig.testf.Pure]] = {
            import _root_.cats.instances.list.catsStdInstancesForList
            _root_.cats.Parallel.parSequence(
              _root_.scala.List[_root_.cats.effect.IO[_root_.io.taig.testf.Test[_root_.io.taig.testf.Pure, _root_.scala.Unit]]](
                ..$autoTests
              )
            ).map(_root_.io.taig.testf.Test.and)
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

    val compiledTests = body.map { tree =>
      q"$tree.interpret"
    }

    (compiledTests, Seq.empty)
  }

//    val (entrypoint, label) = c.prefix.tree match {
//      case q"new ${_}(entrypoint = $entrypoint, label = $label)" =>
//        (c.eval[Boolean](c.Expr(entrypoint)), c.eval[Boolean](c.Expr(label)))
//      case q"new ${_}(entrypoint = $entrypoint)" =>
//        (c.eval[Boolean](c.Expr(entrypoint)), true)
//      case q"new ${_}(label = $label)" =>
//        (true, c.eval[Boolean](c.Expr(label)))
//      case q"new ${_}($arguments)" =>
//        if (arguments.nonEmpty)
//          c.abort(
//            c.enclosingPosition,
//            "Invalid arguments supplied to @AutoTestF. Please use named arguments."
//          )
//        else (true, true)
//      case q"new ${_}" => (true, true)
//    }
//
//    val tree = annottees match {
//      case head :: Nil => head.tree
//      case _           => c.abort(c.enclosingPosition, "Only objects allowed")
//    }
//
//    val result = tree match {
//      case q"$mods object ${name: TermName} extends ..$parents { $self => ..$body }" =>
//        val parent =
//          if (entrypoint) tq"io.taig.testf.TestF" +: parents.tail
//          else parents
//
//        q"""
//        $mods object $name extends ..$parent { $self =>
//          ..$body
//
//          val suite: cats.effect.IO[io.taig.testf.Test[io.taig.testf.Pure]] = {
//            ${filter(c)(body.toList, label)}
//              .map(io.taig.testf.Test.label(${name.encodedName.toString}, _))
//          }
//        }
//        """
//      case _ => c.abort(c.enclosingPosition, "Only object allowed")
//    }
//
//    c.Expr(result)
//  }
//
//  def filter(
//              c: blackbox.Context
//            )(body: List[c.Tree], label: Boolean): c.Expr[IO[Test[IO]]] = {
//    import c.universe._
//
//    val test = typeOf[Test[IO]].typeSymbol
//
//    val tests = body
//      .collect { case field: ValOrDefDef if field.tpt.nonEmpty => field }
//      .filter {
//        case field: DefDef => field.tparams.isEmpty && field.vparamss.isEmpty
//        case _: ValDef     => true
//      }
//      .mapFilter { field =>
//        val tpt = c.typecheck(q"??? : ${field.tpt}").tpe.asInstanceOf[TypeRef]
//        if (tpt.typeSymbol == test) {
//          val test = if (label) autoLabel(c)(field.name) else q"${field.name}"
//          val tq"$name[$f]" = field.tpt
//          Some((f, test))
//        } else None
//      }
//      .map { case (f, test) => q"$test.compile" }
//
//    c.Expr {
//      q"""cats.instances.list.catsStdInstancesForList
//            .sequence(List[cats.effect.IO[io.taig.testf.Test[io.taig.testf.Pure]]](..$tests))
//            .map(io.taig.testf.dsl.allOf)"""
//    }
//  }
//
//  def autoLabel(c: blackbox.Context)(term: c.TermName): c.Tree = {
//    import c.universe._
//    q"io.taig.testf.Test.fallback(${term.decodedName.toString})($term)"
//  }
}
