package repls

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

abstract class REPLBase extends REPL {
  type Base

  private[repls] val regexIdentity: List[Regex]
  private[repls] val regexZero: Regex
  private[repls] val operandsRegex: Regex
  private[repls] val distributiveRegex: Regex

  private[repls] val operatorsRegex = "[+\\-*)(]".r
  private[repls] val variableRegex = "(^[a-zA-Z]+)|(\\s[a-zA-Z]+\\s)|(\\s[a-zA-Z]$)".r
  private[repls] val variablesToValueMap: mutable.Map[String, String] = mutable.Map[String, String]()

  def getPostfixExpression(infixExpression: String): Array[String] = {
    val postfixExpression: ArrayBuffer[String] = ArrayBuffer[String]()
    val expressionParts = ArrayBuffer[(Int, String, Boolean)]()
    operatorsRegex.findAllMatchIn(infixExpression) foreach (x => expressionParts.append((x.start, x.group(0), false)))
    operandsRegex.findAllMatchIn(infixExpression) foreach (x => expressionParts.append((x.start, x.group(0), true)))
    expressionParts.sortInPlace()
    val operatorStack: mutable.Stack[String] = mutable.Stack[String]()
    var index = 0
    while (index < expressionParts.size) {
      val expression = expressionParts(index)
      if (expression._3) {
        postfixExpression.append(expression._2)
      } else {
        if (expression._2.equals(")")) {
          while (operatorStack.nonEmpty && !operatorStack.top.equals("(")) postfixExpression.append(operatorStack.pop())
          operatorStack.pop()
        } else if (expression._2.equals("(")) operatorStack.push(expression._2)
        else {
          while (operatorStack.nonEmpty && priority(operatorStack.top) >= priority(expression._2)) postfixExpression
            .append(operatorStack.pop())
          operatorStack.push(expression._2)
        }
      }
      index += 1
    }
    while (operatorStack.nonEmpty) {
      if (!operatorStack.top.equals("(")) postfixExpression.append(operatorStack.pop())
      else operatorStack.pop()
    }
    postfixExpression.toArray
  }

  def priority(x: String): Int = {
    if (x.equals("+") || x.equals("-")) 1
    else if (x.equals("*")) 2
    else if (x.equals("(")) 0
    else -1
  }

  def evaluate(postfixExpression: Array[String], operatorsRegex: String = "[+\\-*]"): String = {
    val operandStack: mutable.Stack[String] = mutable.Stack[String]()
    var postfixExpressionIndex = 0
    while (postfixExpressionIndex < postfixExpression.length) {
      if (postfixExpression(postfixExpressionIndex).matches(operatorsRegex)) {
        val op2 = operandStack.pop()
        val op1 = operandStack.pop()
        val res =
          if (postfixExpression(postfixExpressionIndex).equals("+"))
            add(op1, op2)
          else if (postfixExpression(postfixExpressionIndex).equals("-"))
            sub(op1, op2)
          else mul(op1, op2)
        operandStack.push(res)
      } else operandStack.push(postfixExpression(postfixExpressionIndex))
      postfixExpressionIndex += 1
    }
    operandStack.pop()
  }

  def solve(command: String): String = {
    if (command.contains("=")) {
      val variableName = command.split("=")(0).trim
      var rhs = command.split("=")(1)
      val variables = variableRegex.findAllIn(command).toArray.map(_.trim)
      variables foreach { variable =>
        rhs = rhs.replaceAll(
          s"(^$variable)|(\\s$variable\\s)|(\\s$variable$$)",
          variablesToValueMap.getOrElse(variable.trim, "{}")
        )
      }
      val postfixExpression = getPostfixExpression(rhs)
      val result = evaluate(postfixExpression)
      variablesToValueMap.put(variableName, result)
      s"$variableName = $result"
    } else {
      var rhs = command
      val variables = variableRegex.findAllIn(command).toArray.map(_.trim)
      variables foreach { variable =>
        rhs = rhs.replaceAll(
          s"(^$variable)|(\\s$variable\\s)|(\\s$variable$$)",
          variablesToValueMap.getOrElse(variable.trim, "{}")
        )
      }
      val postfixExpression = getPostfixExpression(rhs)
      val result = evaluate(postfixExpression)
      result
    }
  }

  def getMinPriority(term: String): Int = {
    if (term.contains("+") || term.contains("-")) priority("+")
    else if (term.contains("*")) priority("*")
    else Int.MaxValue
  }

  def paranthesize(postfixExp: Array[String]): String = {
    val operandStack = mutable.Stack[String]()
    postfixExp foreach (t => {
      if (t.matches(operandsRegex.toString))
        operandStack.push(t)
      else {
        val op2 = operandStack.pop()
        val op1 = operandStack.pop()
        val p2 = getMinPriority(op2)
        val p1 = getMinPriority(op1)
        val operatorPriority = priority(t)
        val pOp1 = if (operatorPriority > p1) s"($op1)" else op1
        val pOp2 = if (operatorPriority > p2) s"($op2)" else op2
        operandStack.push(s"$pOp1 $t $pOp2")
      }
    })
    if (operandStack.nonEmpty) operandStack.pop()
    else ""
  }

  def simplify(exp: String, zero: String = "0", idempotentRegex: Option[Regex] = None): String = {
    var lastExp = ""
    var currentExp = exp
    while (!lastExp.equals(currentExp)) {
      lastExp = currentExp
      regexIdentity.foreach(regex => currentExp = regex.replaceAllIn(currentExp, m => m.subgroups(1)))
      currentExp = currentExp.replaceAll(regexZero.toString(), zero)
    }
    currentExp = distributiveRegex.replaceAllIn(
      currentExp,
      m => {
        if (m.group("f11").equals(m.group("s11"))) s"${m.group("f11")} * (${m.group("f12")} + ${m.group("s12")})"
        else if (m.group("f11").equals(m.group("s12"))) s"${m.group("f11")} * (${m.group("f12")} + ${m.group("s11")})"
        else if (m.group("f12").equals(m.group("s11"))) s"${m.group("f12")} * (${m.group("f11")} + ${m.group("s12")})"
        else if (m.group("f12").equals(m.group("s12"))) s"${m.group("f12")} * (${m.group("f11")} + ${m.group("s11")})"
        else m.source.toString
      }
    )
    if (idempotentRegex.isDefined) {
      currentExp = idempotentRegex.get.replaceAllIn(currentExp, m => {
        if (m.group("o1").equals(m.group("o2"))) m.group("o1")
        else m.group(0).toString
      })
    }
    val postfixExp = getPostfixExpression(currentExp)
    paranthesize(postfixExp)
  }

  def add(op1: String, op2: String): String

  def sub(op1: String, op2: String): String

  def mul(op1: String, op2: String): String

}
