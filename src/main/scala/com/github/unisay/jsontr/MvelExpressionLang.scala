package com.github.unisay.jsontr

import org.mvel2.MVEL

/**
 * MVEL implementation of ExpressionLang
 */
object MvelExpressionLang extends ExpressionLang {

  implicit val mvelEL: ExpressionLang = this

  override def compile(expression: String): Expression = new Expression(MVEL.compileExpression(expression))

  override def test(expression: Expression, node: Node): Boolean =
    MVEL.executeExpression(expression.source, node.asInstanceOf[Object], classOf[java.lang.Boolean])

}
