package com.github.unisay.jsontr

import org.json4s._

object Node {

  def apply(name: String, value: JValue) = new Node(Some(name), value)

  def apply(value: JValue) = new Node(None, value)

  def apply(field: JField) = new Node(Some(field._1), field._2)

  def unapply(node: Node): Option[(Option[String], JValue)] = Some((node._1, node._2))
}

class Node(val maybeName: Option[String], val jsonValue: JValue)
  extends Tuple2[Option[String], JValue](maybeName, jsonValue) {
  def toJField: JField = this match {
    case Node(None, jsonValue: JValue) => ("", jsonValue)
    case Node(Some(thing), jsonValue: JValue) => (thing, jsonValue)
  }

  def value = jsonValue.values
}