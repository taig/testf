object ScalacheckGenerator {
  def apply(pkg: String, name: String): String = {
    s"""package $pkg
       |
       |import com.ayendo.testf._
       |import org.scalacheck.Test.Parameters
       |import org.scalacheck.util.Pretty
       |import org.scalacheck.{Gen, Prop, Shrink}
       |
       |trait $name {
       |${(1 to 8).map(check).mkString("\n")}
       |}
     """.stripMargin
  }

  def types(length: Int): String =
    (1 to length).map(index => s"A$index").mkString(", ")

  def parameters(length: Int): String =
    (1 to length).map(index => s"a$index: Gen[A$index]").mkString(", ")

  def implicits(length: Int): String =
    (1 to length)
      .map(index => s"s$index: Shrink[A$index], pp$index: A$index => Pretty")
      .mkString(", ")

  def argumentsGen(length: Int): String =
    (1 to length).map(index => s"a$index").mkString(", ")

  def argumentsImplicits(length: Int): String =
    (1 to length).map(index => s"s$index, pp$index").mkString(", ")

  def check(length: Int): String = {
    s"""
       |  def check$length[${types(length)}](${parameters(length)}, parameters: Parameters = Parameters.default)(
       |    f: (${types(length)}) => Test[Unit]
       |  )(
       |    implicit
       |    ${implicits(length)}
       |  ): Test[Unit] =
       |    ScalacheckTestBuilders.check(Prop.forAll(${argumentsGen(length)})(f)(_, ${argumentsImplicits(
         length)}))
     """.stripMargin
  }
}
