package repls

import scala.util.matching.Regex

class MultiSetREPL extends REPLBase {
  override type Base = MultiSet[Char]

  override val replName: String = "multisetRepl"

  override val operandsRegex: Regex = "\\{([a-zA-Z0-9\\.,]*)\\}".r

  override val regexIdentity: List[Regex] =
    List[Regex]("(\\{\\} \\+ (\\{([a-zA-Z0-9\\.,]*)\\}))".r, "((\\{([a-zA-Z0-9\\.,]*)\\}) \\+ \\{\\})".r)

  override val regexZero: Regex = "(\\{\\} \\* \\{([a-zA-Z0-9\\.,]*)\\})|(\\{([a-zA-Z0-9\\.,]*)\\} \\* \\{\\})".r

  override val distributiveRegex: Regex =
    "\\( (\\{[a-zA-Z0-9\\.,]*\\}) \\* (\\{[a-zA-Z0-9\\.,]*\\}) \\) \\+ \\( (\\{[a-zA-Z0-9\\.,]*\\}) \\* (\\{[a-zA-Z0-9\\.,]*\\}) \\)"
      .r("f11", "f12", "s11", "s12")

  val idempotentRegex: Regex = "(\\{[a-zA-Z0-9\\.,]*\\}) \\* (\\{[a-zA-Z0-9\\.,]*\\})".r("o1", "o2")

  override def readEval(command: String): String = {
    if (command.startsWith("@")) {
      val exp =
        operandsRegex.replaceAllIn(command.substring(1), m => getMultiSetFromString(m.group(0).toString).toString)

      simplify(exp, "{}", Some(idempotentRegex))
    } else solve(command)
  }

  def getMultiSetFromString(exp: String): MultiSet[Char] = {
    val elements = exp.substring(1, exp.length - 1).trim
    if (elements.length == 0)
      MultiSet[Char](Seq.empty)
    else
      MultiSet[Char](elements.split(",").map(_(0)))
  }

  override def add(op1: String, op2: String): String = {
    val set1 = getMultiSetFromString(op1)
    val set2 = getMultiSetFromString(op2)
    (set1 + set2).toString
  }

  override def sub(op1: String, op2: String): String = {
    val set1 = getMultiSetFromString(op1)
    val set2 = getMultiSetFromString(op2)
    (set1 - set2).toString
  }

  override def mul(op1: String, op2: String): String = {
    val set1 = getMultiSetFromString(op1)
    val set2 = getMultiSetFromString(op2)
    (set1 * set2).toString
  }

}

object MultiSetREPL {

  def main(args: Array[String]): Unit = {
    val repl = new MultiSetREPL()
    repl.run()
  }

}
