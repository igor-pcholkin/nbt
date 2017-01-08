package com.random.nbt

import java.io.File
import scala.collection.mutable.Map

object Context {
  val internal = create

  def get(names: String*): Option[Any] = {
    names.dropWhile(internal.get(_).isEmpty).headOption map (internal(_))
  }

  def getString(names: String*) = get(names: _*) flatMap { value =>
    value match {
      case s: String => Some(s)
      case _ => None
    }
  }

  def set(name: String, value: Any) = internal += (name -> value)

  def getKeys() = internal.keys

  private def create = {
    val currentDir = new File(".").getCanonicalPath
    val projectName = currentDir.substring(currentDir.lastIndexOf(File.separator) + 1)
    Map[String, Any](
        "currentDir" -> currentDir,
        "projectName" -> projectName
        )
  }
}
