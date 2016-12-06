package com.random.nbt

import java.io.File
import scala.collection.mutable.Map

object Context {
  val internal = create

  def get(names: String*): Option[Any] = {
    names.dropWhile(internal.get(_).isEmpty).headOption map (internal(_))
  }

  def getString(names: String*) = get(names: _*) map (_.asInstanceOf[String])

  def set(name: String, value: String) = internal += (name -> value)

  def set(name: String, values: Seq[String]) = internal += (name -> values)

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
