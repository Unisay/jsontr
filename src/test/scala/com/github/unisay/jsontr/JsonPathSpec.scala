package com.github.unisay.jsontr

import com.github.unisay.jsontr.parser._
import org.specs2.mutable.Specification

class JsonPathSpec extends Specification {

  "JsonPath.eval" should {

    val json = NodeParser.parse(
      """
        |{
        |  "title": "Forrest Gump",
        |  "description": null,
        |  "meta": {
        |    "running-time": 142.0,
        |    "year": 1994,
        |    "best": true,
        |    "authors": {
        |      "director": {
        |        "name": "Robert",
        |        "surname": "Zemeckis"
        |      },
        |      "producer": {
        |        "name": "Wendy",
        |        "surname": "Finerman"
        |      }
        |    }
        |  },
        |  "heroes": [
        |    { "name": "Forrest", "surname": "Gump"   },
        |    { "name": "Dan",     "surname": "Taylor" },
        |    { "name": "Jenny",   "surname": "Curan"  }
        |  ]
        |}
      """.stripMargin
    )

    "return None for empty path" ! (JsonPath.eval("", json) must be empty)

    "return None for blank path" ! (JsonPath.eval(" ", json) must be empty)

    "return a root document by /" in {
      JsonPath.eval("/", json) must contain(exactly(json))
    }

    "return a string node by absolute path" in {
      val title: Node = SNode("title", "Forrest Gump")
      JsonPath.eval("/title", json) must contain(exactly(title))
    }

    "return a object node by absolute path" in {
      val json = NodeParser.parse(
        """
          |{
          |  "a": {
          |   "b": "c"
          |  }
          |}
        """.stripMargin
      )
      val node: Node = ONode("a", List(SNode("b", "c")))
      JsonPath.eval("/a", json) must contain(exactly(node))
    }

    "return a top field by relative path" in {
      val title: Node = SNode("title", "Forrest Gump")
      JsonPath.eval("title", json) must contain(exactly(title))
    }

    "return a nested field by absolute path" in {
      val name: Node = SNode("name", "Robert")
      JsonPath.eval("/meta/authors/director/name", json) must contain(exactly(name))
    }

    "return a nested field by relative path" in {
      val surname: Node = SNode("surname", "Zemeckis")
      JsonPath.eval("meta/authors/director/surname", json) must contain(exactly(surname))
    }

    "return all object fields" in {
      JsonPath.eval("meta/authors/director/*", json) must contain(exactly[Node](
        SNode("name", "Robert"),
        SNode("surname", "Zemeckis")
      ))
    }

    "return array element by index" in {
      JsonPath.eval("/2", NodeParser.parse( """[ "a", "b", "c" ]""")) must contain(exactly[Node](SNode("c")))
    }

    "return all array elements" in {
      JsonPath.eval("/*", NodeParser.parse( """[ "a", "b", "c" ]""")) must contain(exactly[Node](
        SNode("a"),
        SNode("b"),
        SNode("c")
      ))
    }

    "return by index from all elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  [ 4, 5, 6 ],
          |  [ 7, 8, 9 ]
          |]
        """.stripMargin
      JsonPath.eval("/*/1", NodeParser.parse(json)) must contain(exactly[Node](INode(2), INode(5), INode(8)))
    }

    "return by index from all array elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  [ 4, 5, 6 ],
          |  [ 7, 8, 9 ],
          |  { "a": 10 }
          |]
        """.stripMargin
      JsonPath.eval("/*/1", NodeParser.parse(json)) must contain(exactly[Node](INode(2), INode(5), INode(8)))
    }

    "return by property from all object elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  { "a": 10 },
          |  { "a": 11 }
          |]
        """.stripMargin
      JsonPath.eval("/*/a", NodeParser.parse(json)) must contain(exactly[Node](INode("a", 10), INode("a", 11)))
    }

    "return all array elements" in {
      JsonPath.eval("heroes/*", json) must contain(exactly[Node](
        ONode(List(SNode("name", "Forrest"), SNode("surname", "Gump"))),
        ONode(List(SNode("name", "Dan"), SNode("surname", "Taylor"))),
        ONode(List(SNode("name", "Jenny"), SNode("surname", "Curan")))
      ))
    }

    "return a property from all array elements" in {
      JsonPath.eval("heroes/*/name", json) must contain(exactly[Node](
        SNode("name", "Forrest"),
        SNode("name", "Dan"),
        SNode("name", "Jenny")
      ))
    }

    "return filtered fields" in {
      JsonPath.eval("heroes/*/name[value.length > 3 && value.startsWith('F')]", json) must
        contain(exactly[Node](SNode("name", "Forrest")))
    }

  }

}
