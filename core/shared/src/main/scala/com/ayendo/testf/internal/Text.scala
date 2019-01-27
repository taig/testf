package com.ayendo.testf.internal

import scala.compat.Platform.EOL

object Text {
  def colorize(value: String, color: String): String =
    value
      .split(EOL)
      .map(value => color + value + Console.RESET)
      .mkString(EOL)

  def colorizeCond(value: String, color: String, enabled: Boolean): String =
    if (enabled) colorize(value, color) else value

  def padLeft(value: String, columns: Int): String =
    value
      .split(EOL)
      .map(value => (" " * columns) + value)
      .mkString(EOL)
}
