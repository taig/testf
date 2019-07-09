package com.ayendo.testf.internal

object Text {
  def colorize(value: String, color: String): String =
    value
      .split(System.lineSeparator)
      .map(value => color + value + Console.RESET)
      .mkString(System.lineSeparator)

  def colorizeCond(value: String, color: String, enabled: Boolean): String =
    if (enabled) colorize(value, color) else value

  def padLeft(value: String, columns: Int): String =
    value
      .split("\n")
      .map(value => (" " * columns) + value)
      .mkString(System.lineSeparator)
}
