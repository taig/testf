package com.ayendo.testf.internal

object Options {
  def when[A](cond: Boolean)(a: => A): Option[A] =
    if (cond) Some(a) else None
}
