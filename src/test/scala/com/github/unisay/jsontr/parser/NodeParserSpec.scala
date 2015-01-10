package com.github.unisay.jsontr.parser

import com.github.unisay.jsontr.parser.NodeParser.parse
import org.specs2.mutable.Specification


class NodeParserSpec extends Specification {

  "NodeParser" should {

    "parse empty array" in {
      parse("[]") must be equalTo ANode()
    }

    "parse array of array" in {
      parse("[[]]") must be equalTo ANode(List(ANode()))
    }

    "parse array of scalars" in {
      parse( """[null, -1, 2.0, "a", true, false]""") must be equalTo ANode(
        List(NullNode(), INode(BigInt(-1)), FNode(2.0), SNode("a"), BNode(value = true), BNode(value = false))
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

  }

}
