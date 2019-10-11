package com.ayendo.testf.internal

import java.io.{PrintWriter, StringWriter}

object Text {
  def colorize(value: String, color: String): String =
    value
      .split("\n")
      .map(value => color + value + Console.RESET)
      .mkString("\n")

  def padLeft(value: String, columns: Int): String =
    value
      .split("\n")
      .map(value => (" " * columns) + value)
      .mkString("\n")

  def print(throwable: Throwable): String = {
    val writer = new StringWriter
    throwable.printStackTrace(new PrintWriter(writer))
    writer.toString
  }
}
