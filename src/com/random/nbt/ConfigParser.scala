package com.random.nbt

import scala.io.Source
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

case class Phase(name: String, cmdLines: List[String], description: Option[String], dependsOn: List[DependantPhase], calls: List[String])
case class DependantPhase(name: String)

class Attribute

case class Command(cmdLine: String) extends Attribute
case class Call(lines: String) extends Attribute
case class Description(value: String) extends Attribute
case class Depends(phases: List[DependantPhase]) extends Attribute

class ConfigParser extends JavaTokenParsers {

  lazy val eol = sys.props("line.separator")

  override val whiteSpace = """[ \t]+""".r

  def str = ("""([^\n\r])+""").r ^^ { s => s.trim() }

  def delimitedStr = ("""([^\n\r,])+""").r ^^ { s => s.trim() }

  def phaseName = ("""([^\n\r:])+""").r

  def phaseNameHeader = phaseName <~ rep1(":", eol)

  def description = "description" ~ ":" ~> str ^^ { value =>
    Description(value)
  }

  def dependsOn = ("depends on" ~ ":") ~> rep1sep(delimitedStr, ",") ^^ { values =>
    Depends(values.map(DependantPhase(_)))
  }

  def command = ("command" ~ ":") ~> str ^^ { s => Command(s) }

  def call = ("call" ~ ":") ~> str ^^ { params =>
    Call(params)
  }

  def attribute = (description | dependsOn | command | call) <~ eol

  def getCommandLines(attributes:List[Attribute]) = attributes collect { case cmd: Command => cmd.cmdLine }
  def getCalls(attributes:List[Attribute]) = attributes collect { case c: Call => c.lines }
  def getValueOfFirstDescription(attributes:List[Attribute]) = (attributes collect { case d: Description => d.value }).headOption
  def getFirstDependsOnList(attributes:List[Attribute]) = (attributes collect { case d: Depends => d.phases }).headOption.getOrElse(Nil)

  def phase = (phaseNameHeader ~ rep(attribute)) <~ rep1(eol) ^^ {
    case name ~ attributes =>
      val cmdLines = getCommandLines(attributes)
      val description = getValueOfFirstDescription(attributes)
      val dependsOn = getFirstDependsOnList(attributes)
      val call = getCalls(attributes)
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
