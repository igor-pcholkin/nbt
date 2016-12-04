package com.random.nbt

import java.io.File
import scala.collection.mutable.Map

class ContextCreator {
  def create = {
    val currentDir = new File(".").getCanonicalPath
    val projectName = currentDir.substring(currentDir.lastIndexOf(File.separator) + 1)
    Map[String, Any](
        "currentDir" -> currentDir,
        "projectName" -> projectName
        )
  }
}
