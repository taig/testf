package com.ayendo.testf

import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
abstract class TestF {
  def suite: List[Assert]
}
