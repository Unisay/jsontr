package com.github.unisay.jsontr.parser

import com.github.unisay.jsontr.parser.NodeWriter.write
import org.specs2.mutable.Specification


class NodeWriterSpec extends Specification {

  "NodeWriter" should {

    "write string containing quote" in {
      write(SNode("\"a", "\"b")) must be equalTo """{"\"a":"\"b"}"""
    }

    "write string nested in an object" in {
      val node = ONode(List(
        SNode(Some("a"), "b")
      ))
      write(node) must be equalTo """{"a":"b"}"""
    }

    "write array nested in an object" in {
      val node = ONode(List(
        ONode("nested", List(
          ANode("a", List(
            INode(1), INode(2)
          ))))))

      write(node) must be equalTo """{"nested":{"a":[1,2]}}"""
    }

  }

}
