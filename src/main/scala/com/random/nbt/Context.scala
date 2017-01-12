package com.random.nbt

import java.io.File
import scala.collection.mutable.Map

object Context {
  val DEPENDENCIES = "deps"
}

class Context {
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

  def setRaw(varName: String, rawValue: String) = {
    val value: Any = if (rawValue.contains(","))
      rawValue.split("[,\\s]+").toSeq
    else if (varName == Context.DEPENDENCIES)
      Seq(rawValue)
    else
      rawValue
    set(varName, value)
  }

  def getKeys() = internal.keys

  private def create = {
    val currentDir = new File(".").getCanonicalPath
    val projectName = currentDir.substring(currentDir.lastIndexOf(File.separator) + 1)
    Map[String, Any](
        "currentDir" -> currentDir,
        "projectName" -> projectName
        )
  }

  def resolveVarsIn(line: String) = {
    if (line.contains("$")) {
      getKeys.toSeq.sortBy(-_.length).foldLeft(line) { (cmdLine, key) =>
        val value = get(key) match {
          case Some(value: String) => value
          case _ => ""
        }
        cmdLine.replace("$" + s"$key", value)
      }
    } else {
      line
    }
  }


}
