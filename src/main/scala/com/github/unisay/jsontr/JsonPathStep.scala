package com.github.unisay.jsontr

sealed trait JsonPathStep {
  def predicate(): Option[Expression]
}

case object / extends JsonPathStep {
  override def predicate() = None
}

case class All(predicate: Option[Expression] = None) extends JsonPathStep

case class Prop(field: String, predicate: Option[Expression] = None) extends JsonPathStep

case class Index(index: Int, predicate: Option[Expression] = None) extends JsonPathStep
