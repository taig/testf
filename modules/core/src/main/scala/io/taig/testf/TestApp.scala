package io.taig.testf

import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
trait TestApp:
  final def main(arguments: Array[String]): Unit = main(System.out.println(_: String), _ => ())

  def main(logger: String => Unit, callback: Option[Report] => Unit): Unit
