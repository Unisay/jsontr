package com.github.unisay.jsontr.parser

import com.github.unisay.jsontr.parser.NodeWriter.write
import org.specs2.mutable.Specification


class NodeWriterSpec extends Specification {

  "NodeWriter" should {

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
