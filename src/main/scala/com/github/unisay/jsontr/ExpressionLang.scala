package com.github.unisay.jsontr

case class Expression(source: Any)

/**
 * Expression Language
 */
trait ExpressionLang {

  def compile(expression: String): Expression

  def test(predicate: Expression, node: Node): Boolean

  def applyPredicate(predicate: Expression, node: Node): Option[Node] = if (test(predicate, node)) Some(node) else None

}
