package com.random.nbt
import scala.xml._
import java.io.PrintWriter
import java.io.FileWriter

class EclipseManager(implicit context: Context) {
  def createProject() = {
    val projectName = context.getString("projectName").getOrElse("test")
    val dependencies = context.getString("dependenciesAsJarPaths").getOrElse("").split(":")
    val workingDir = context.getString("currentDir").getOrElse(".")
    writeProject(projectName, workingDir)
    writeClassPath(dependencies, workingDir)
    mkDir(s"$workingDir/src")
    mkDir(s"$workingDir/bin")
  }

  def writeProject(projectName: String, workingDir: String) = {
    writeXML(s"$workingDir/.project",
    <projectDescription>
      <name>{projectName}</name>
      <comment></comment>
      <projects>
      </projects>
      <buildSpec>
        <buildCommand>
          <name>org.scala-ide.sdt.core.scalabuilder</name>
          <arguments>
          </arguments>
        </buildCommand>
      </buildSpec>
      <natures>
        <nature>org.scala-ide.sdt.core.scalanature</nature>
        <nature>org.eclipse.jdt.core.javanature</nature>
      </natures>
    </projectDescription>
    )
  }

  def writeClassPath(dependencies: Seq[String], workingDir: String) = {
    writeXML(s"$workingDir/.classpath",
    <classpath>
       <classpathentry kind="src" path="src"/>
       <classpathentry kind="src" path="conf"/>
       <classpathentry kind="con" path="org.eclipse.jdt.launching.JRE_CONTAINER"/>
       <classpathentry kind="con" path="org.scala-ide.sdt.launching.SCALA_CONTAINER"/>
       {
         dependencies.collect { case dep if dep.nonEmpty =>
           <classpathentry kind="lib" path={dep}/>
         }
       }
       <classpathentry kind="output" path="bin"/>
    </classpath>)
  }

  def writeXML(fileName: String, xml: Elem) = {
    val pw = new PrintWriter(new FileWriter(fileName))
    pw.println("""<?xml version="1.0" encoding="UTF-8"?>""")
    val printer = new scala.xml.PrettyPrinter(120, 2)
    pw.println(printer.format(xml))
    pw.close()
  }

  def mkDir(dir: String) = {
    CommandLineExecutor.execute(s"mkdir -p $dir")
  }
}
