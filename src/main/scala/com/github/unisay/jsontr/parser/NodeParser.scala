package com.github.unisay.jsontr.parser

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonToken}

import scala.collection.mutable.ListBuffer

sealed trait Node {
  val name: Option[String]
}

trait MultiNode extends Node

case class NullNode(name: Option[String] = None) extends Node

case object NullNode {
  def apply(name: String) = new NullNode(Some(name))
}

case class SNode(name: Option[String], value: String) extends Node

case object SNode {

  def apply(name: String, value: String) = new SNode(Some(name), value)

  def apply(value: String) = new SNode(None, value)
}

case class BNode(name: Option[String], value: Boolean) extends Node

case object BNode {

  def apply(name: String, value: Boolean) = new BNode(Some(name), value)

  def apply(value: Boolean) = new BNode(None, value)
}

case class INode(name: Option[String], value: BigInt) extends Node

case object INode {

  def apply(name: String, value: BigInt) = new INode(Some(name), value)

  def apply(value: BigInt) = new INode(None, value)
}

case class FNode(name: Option[String], value: Double) extends Node

case object FNode {

  def apply(name: String, value: Double) = new FNode(Some(name), value)

  def apply(value: Double) = new FNode(None, value)
}

case class ANode(name: Option[String], children: Seq[Node]) extends MultiNode

case object ANode {

  def apply(name: String, children: Seq[Node]) = new ANode(Some(name), children)

  def apply(children: Seq[Node] = Seq.empty) = new ANode(None, children)
}

case class ONode(name: Option[String], children: Seq[Node]) extends MultiNode

case object ONode {

  def apply(name: String, children: Seq[Node]) = new ONode(Some(name), children)

  def apply(children: Seq[Node] = Seq.empty) = new ONode(None, children)
}

object NodeParser {

  val jsonFactory = new JsonFactory()

  def parse(json: String): Node = {
    val parser = jsonFactory.createParser(json)
    try {
      parseMultiNode(None, parser)
    } finally {
      parser.close()
    }
  }

  private def parseMultiNode(name: Option[String], parser: JsonParser): MultiNode = {
    parser.nextToken match {
      case JsonToken.START_OBJECT => parseObject(name, parser)
      case JsonToken.START_ARRAY => parseArray(name, parser)
      case _ => throw new IllegalStateException("Either { or [ expected")
    }
  }

  private def parseNode(name: Option[String], parser: JsonParser): Node = {
    val node = parser.getCurrentToken match {
      case JsonToken.VALUE_NULL => NullNode(name)
      case JsonToken.VALUE_FALSE => BNode(name, value = false)
      case JsonToken.VALUE_TRUE => BNode(name, value = true)
      case JsonToken.VALUE_STRING => SNode(name, parser.getText)
      case JsonToken.VALUE_NUMBER_INT => INode(name, parser.getBigIntegerValue)
      case JsonToken.VALUE_NUMBER_FLOAT => FNode(name, parser.getDoubleValue)
      case JsonToken.START_ARRAY => parseArray(name, parser)
      case JsonToken.START_OBJECT => parseObject(name, parser)
      case _ => throw new IllegalStateException("Scalar value expected but got " + parser.getCurrentToken)
    }
    parser.nextToken
    node
  }

  private def parseArray(name: Option[String], parser: JsonParser): ANode = {
    var currentToken = parser.nextToken
    val nodes = ListBuffer[Node]()
    while (currentToken != JsonToken.END_ARRAY) {
      nodes += parseNode(None, parser)
      currentToken = parser.getCurrentToken
    }
    ANode(name, nodes)
  }

  private def parseObject(name: Option[String], parser: JsonParser): ONode = {
    var currentToken = parser.nextToken()
    val nodes = ListBuffer[Node]()
    while (currentToken != JsonToken.END_OBJECT) {
      nodes += parseField(parser)
      currentToken = parser.getCurrentToken
    }
    ONode(name, nodes)
  }

  private def parseField(parser: JsonParser): Node = {
    val fieldName = parser.getText
    parser.nextToken()
    parseNode(Some(fieldName), parser)
  }

}
