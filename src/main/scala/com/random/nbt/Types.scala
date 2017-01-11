package com.random.nbt

import shapeless.TypeCase

object Types {
  val SomeSeqString = TypeCase[Option[Seq[String]]]
  val SomeMapString = TypeCase[Option[Map[String, String]]]
}
