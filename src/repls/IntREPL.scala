package repls

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack, StringBuilder}
import scala.util.matching.Regex

class IntREPL extends REPLBase {
  type Base = Int

  override val regexIdentity: List[Regex] =
    List[Regex]("(0 \\+ (\\d+))".r, "((\\d+) \\+ 0)".r, "((\\d+) \\* 1)".r, "(1 \\* (\\d+))".r)

  override val regexZero: Regex = "(0 \\* \\d+)|(\\d+ \\* 0)".r

  override val distributiveRegex: Regex =
    "\\( (\\d+) \\* (\\d+) \\) \\+ \\( (\\d+) \\* (\\d+) \\)".r("f11", "f12", "s11", "s12")

  override val replName: String = "IntRepl"

  override private[repls] val operandsRegex = "[0-9]+".r

  override def add(op1: String, op2: String): String = (op1.toInt + op2.toInt).toString

  override def sub(op1: String, op2: String): String = (op1.toInt - op2.toInt).toString

  override def mul(op1: String, op2: String): String = (op1.toInt * op2.toInt).toString

  override def readEval(command: String): String = {
    if (command.startsWith("@")) simplify(command.substring(1), "0")
    else solve(command)
  }

}

object IntREPL {

  def main(args: Array[String]): Unit = {
    val intREPL = new IntREPL()
    intREPL.run()
  }

}
