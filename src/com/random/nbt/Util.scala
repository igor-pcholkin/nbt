package com.random.nbt

object Util {
  def grouped[A, B](s: Seq[(A, B)]) = s groupBy (_._1) mapValues (_.map(_._2))

}
