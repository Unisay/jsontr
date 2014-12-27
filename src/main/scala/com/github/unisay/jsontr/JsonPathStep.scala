package com.github.unisay.jsontr

import java.io.Serializable

sealed trait JsonPathStep {
  def predicate(): Option[Serializable]
}

case object / extends JsonPathStep {
  override def predicate() = None
}

case class All(predicate: Option[Serializable] = None) extends JsonPathStep

case class Prop(field: String, predicate: Option[Serializable] = None) extends JsonPathStep

case class Index(index: Int, predicate: Option[Serializable] = None) extends JsonPathStep
