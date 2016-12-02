package com.random.nbt

import scala.io.Source
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

case class Phase(name: String, cmdLines: List[String], description: Option[String], dependsOn: List[DependantPhase], calls: List[String])
case class DependantPhase(name: String)

class Attribute

case class Command(cmdLine: String) extends Attribute
case class Call(lines: List[String]) extends Attribute
case class Description(value: String) extends Attribute
case class Depends(phases: List[DependantPhase]) extends Attribute

case class Commands(commands: List[Command]) extends Attribute

class ConfigParser extends JavaTokenParsers {

  lazy val eol = sys.props("line.separator")

  override val whiteSpace = """[ \t\n]+""".r

  def str = ("""([^\n\r])+""").r ^^ { s => s.trim()
  }

  def phaseName = ("clean" | "compile" | "package" | "run") <~ ":"

  def description = "description" ~ ":" ~> str ^^ { value =>
    Description(value)
  }

  def dependsOn = "depends on" ~ ":" ~> rep1(str, ",") ^^ { values =>
    Depends(values.map(DependantPhase(_)))
  }

  def commands = "commands" ~ ":" ~> rep1(str, eol) ^^ { cmdLines =>
    Commands(cmdLines.map(Command(_)))
  }

  def call = "call" ~ ":" ~> rep1(str, eol) ^^ { params =>
    Call(params)
  }

  def attribute = description | dependsOn | commands | call

  def getCommandLineOfFirstCommands(attributes:List[Attribute]) = (attributes collect { case cmds: Commands => cmds.commands }).headOption.getOrElse(Nil) map { cmd => cmd.cmdLine }
  def getValueOfFirstCall(attributes:List[Attribute]) = (attributes collect { case c: Call => c.lines }).headOption.getOrElse(Nil)
  def getValueOfFirstDescription(attributes:List[Attribute]) = (attributes collect { case d: Description => d.value }).headOption
  def getFirstDependsOnList(attributes:List[Attribute]) = (attributes collect { case d: Depends => d.phases }).headOption.getOrElse(Nil)

  def phase = phaseName ~ rep(attribute) ^^ {
    case name ~ attributes =>
      val cmdLines = getCommandLineOfFirstCommands(attributes)
      val description = getValueOfFirstDescription(attributes)
      val dependsOn = getFirstDependsOnList(attributes)
      val call = getValueOfFirstCall(attributes)
      Phase(name, cmdLines, description, dependsOn, call)
  }

  def config = rep1(phase)

  def parse(configFile: String = "conf/default.conf") = {
    parseAll(config, new FileReader(configFile)) match {
      case Success(phases, _) => phases
      case ex @ _             => println(ex); List[Phase]()
    }
  }
}
