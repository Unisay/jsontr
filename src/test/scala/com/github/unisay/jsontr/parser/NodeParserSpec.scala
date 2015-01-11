package com.github.unisay.jsontr.parser

import com.github.unisay.jsontr.parser.NodeParser.parse
import org.specs2.mutable.Specification


class NodeParserSpec extends Specification {

  "NodeParser" should {

    "parse null" in {
      parse("null") must throwA[IllegalStateException]
    }

    "parse empty array" in {
      parse("[]") must be equalTo ANode()
    }

    "parse array of array" in {
      parse("[[]]") must be equalTo ANode(List(ANode()))
    }

    "parse array of object" in {
      parse("[{}]") must be equalTo ANode(List(ONode()))
    }

    "parse array of scalars" in {
      parse( """[null, -1, 2.0, "a", true, false]""") must be equalTo ANode(
        List(NullNode(), INode(BigInt(-1)), DNode(2.0), SNode("a"), BNode(value = true), BNode(value = false))
      )
    }

    "parse empty object" in {
      parse("{}") must be equalTo ONode()
    }

    "parse object" in {
      parse( """{ "prop": "val" }""") must be equalTo ONode(List(SNode("prop", "val")))
    }

    "parse nested object" in {
      parse( """{ "nested": { "a": "b" } }""") must be equalTo ONode(List(ONode("nested", List(SNode("a", "b")))))
    }

    "parse array nested in an object" in {
      parse( """{ "nested": { "a": [1, 2] } }""") must be equalTo ONode(List(
        ONode("nested", List(
          ANode("a", List(
            INode(1), INode(2)
          ))))))
    }

  }

}
