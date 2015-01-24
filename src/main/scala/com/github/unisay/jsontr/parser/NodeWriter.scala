package com.github.unisay.jsontr.parser

/**
 * Serializes AST to string
 */
object NodeWriter {

  def write(node: Node): String = {

    def writeField(nameOption: Option[String], value: String) = nameOption match {
      case None => value
      case Some(name) => "\"" + name + "\":" + value
    }

    node match {
      case NullNode(nameOption) => writeField(nameOption, "null")
      case INode(nameOption, value) => writeField(nameOption, value.toString())
      case BNode(nameOption, value) => writeField(nameOption, value.toString)
      case DNode(nameOption, value) => writeField(nameOption, value.toString)
      case SNode(nameOption, value) => writeField(nameOption, "\"" + value + "\"")
      case multiNode: MultiNode => multiNode match {
        case ANode(nameOption, children) => writeField(nameOption, "[" + children.map(write).mkString(",") + "]")
        case ONode(nameOption, children) => writeField(nameOption, "{" + children.map(write).mkString(",") + "}")
      }
    }
  }

}
