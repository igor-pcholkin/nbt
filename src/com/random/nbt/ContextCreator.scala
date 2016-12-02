package com.random.nbt

import java.io.File

class ContextCreator {
  def create = {
    val currentDir = new File(".").getCanonicalPath
    val projectName = currentDir.substring(currentDir.lastIndexOf(File.separator) + 1)
    Map[String, String](
        "currentDir" -> currentDir,
        "projectName" -> projectName
        )
  }
}