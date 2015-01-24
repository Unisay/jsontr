package com.github.unisay.jsontr.parser

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonToken}

import scala.collection.mutable.ListBuffer

sealed trait Node {
  val nameOptional: Option[String]

  def detached: Node
}

object Node {
  def unapply(x: Node): Option[String] = x.nameOptional
}

trait MultiNode extends Node {
  val children: Seq[Node]
}

object MultiNode {
  def unapply(x: MultiNode): Option[(Option[String], Seq[Node])] = Some((x.nameOptional, x.children))
}

case class NullNode(nameOptional: Option[String] = None) extends Node {
  def detached = NullNode(None)
}

case object NullNode {
  def apply(name: String) = new NullNode(Some(name))
}

case class SNode(nameOptional: Option[String], value: String) extends Node {
  def detached = SNode(None, value)
}

case object SNode {

  def apply(name: String, value: String) = new SNode(Some(name), value)

  def apply(value: String) = new SNode(None, value)

}

case class BNode(nameOptional: Option[String], value: Boolean) extends Node {
  def detached = BNode(None, value)
}

case object BNode {

  def apply(name: String, value: Boolean) = new BNode(Some(name), value)

  def apply(value: Boolean) = new BNode(None, value)

}

case class INode(nameOptional: Option[String], value: BigInt) extends Node {
  def detached = INode(None, value)
}

case object INode {

  def apply(name: String, value: BigInt) = new INode(Some(name), value)

  def apply(value: BigInt) = new INode(None, value)

}

case class DNode(nameOptional: Option[String], value: Double) extends Node {
  def detached = DNode(None, value)
}

case object DNode {

  def apply(name: String, value: Double) = new DNode(Some(name), value)

  def apply(value: Double) = new DNode(None, value)
}

case class ANode(nameOptional: Option[String], children: Seq[Node]) extends MultiNode {
  def detached = ANode(None, children)
}

case object ANode {

  def apply(name: String, children: Seq[Node]) = new ANode(Some(name), children)

  def apply(children: Seq[Node] = Seq.empty) = new ANode(None, children)
}

case class ONode(nameOptional: Option[String], children: Seq[Node]) extends MultiNode {
  def detached = ONode(None, children)
}

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
      case JsonToken.VALUE_NUMBER_FLOAT => DNode(name, parser.getDoubleValue)
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
