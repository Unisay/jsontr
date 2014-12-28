package com.github.unisay.jsontr

case class Expression(source: Any)

object Expression {
  implicit def stringToExpression(s: String)(implicit el: ExpressionLang): Expression = el.compile(s)
}

/**
 * Expression Language
 */
trait ExpressionLang {

  def compile(expression: String): Expression

  def test(predicate: Expression, node: Node): Boolean

  def applyPredicate(predicate: Expression, node: Node): Option[Node] = if (test(predicate, node)) Some(node) else None

}
