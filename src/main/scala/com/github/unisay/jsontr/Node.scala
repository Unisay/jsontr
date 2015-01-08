package com.github.unisay.jsontr

import org.json4s.JsonAST.JValue
import org.json4s._

object Node {

  type Nodes = Seq[Node]

  implicit def toJField(node: Node): JField = node match {
    case Node(None, jsonValue: JValue) => ("", jsonValue)
    case Node(Some(thing), jsonValue: JValue) => (thing, jsonValue)
  }

  implicit def jsonValueToNodes(value: JValue): Nodes = {
    require(value != null, "value is null")
    List(Node(value))
  }

  implicit def jsonValuesToNodes(values: Seq[JValue]): Nodes = {
    require(values != null, "values is null")
    values.map((field) => Node(field))
  }

  implicit def jsonFieldsToNodes(fields: Seq[JField]): Nodes = fields.map((field) => Node(field))

  implicit def nodesToJsonFields(nodes: Iterable[Node]): Iterable[JField] = nodes.map(toJField)

  implicit def nodesToJsonValues(nodes: Iterable[Node]): Iterable[JValue] = nodes.map(_.jsonValue)

  def apply(maybeName: Option[String], value: JValue) = new Node(maybeName, value)

  def apply(name: String, value: JValue) = new Node(Some(name), value)

  def apply(value: JValue) = new Node(None, value)

  def apply(field: JField) = new Node(Some(field._1), field._2)

  def unapply(node: Node): Option[(Option[String], JValue)] = Some((node._1, node._2))
}

class Node(val maybeName: Option[String], val jsonValue: JValue)
  extends Tuple2[Option[String], JValue](maybeName, jsonValue) {

  def value = jsonValue.values
}
