package com.github.unisay.jsontr

import org.json4s.JsonAST._
import org.json4s.native.JsonMethods._
import org.specs2.mutable.Specification

class JsonPathSpec extends Specification {

  "JsonPath.eval" should {

    val json: JValue = parse(
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

    "require a non-null doc" in {
      JsonPath.eval(null, "/") must throwA[IllegalArgumentException]
    }

    "require a non-null path" in {
      JsonPath.eval(json, null.asInstanceOf[String]) must throwA[IllegalArgumentException]
    }

    "return None for empty path" ! (JsonPath.eval(json, "") must be empty)

    "return None for blank path" ! (JsonPath.eval(json, " ") must be empty)

    "return a root document by /" in {
      JsonPath.eval(json, "/") must contain(exactly(Node(json)))
    }

    "return a top field by absolute path" in {
      JsonPath.eval(json, "/title") must contain(exactly(Node("title", JString("Forrest Gump"))))
    }

    "return a top field by relative path" in {
      JsonPath.eval(json, "title") must contain(exactly(Node("title", JString("Forrest Gump"))))
    }

    "return a nested field by absolute path" in {
      JsonPath.eval(json, "/meta/authors/director/name") must contain(exactly(Node("name", JString("Robert"))))
    }

    "return a nested field by relative path" in {
      JsonPath.eval(json, "meta/authors/director/surname") must contain(exactly(Node("surname", JString("Zemeckis"))))
    }

    "return all object fields" in {
      JsonPath.eval(json, "meta/authors/director/*") must contain(exactly(
        Node("name", JString("Robert")),
        Node("surname", JString("Zemeckis"))
      ))
    }

    "return array element by index" in {
      JsonPath.eval(parse( """[ "a", "b", "c" ]"""), "/2") must contain(exactly(Node(JString("c"))))
    }

    "return all array elements" in {
      JsonPath.eval(parse( """[ "a", "b", "c" ]"""), "/*") must contain(exactly(
        Node(JString("a")),
        Node(JString("b")),
        Node(JString("c"))
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
      JsonPath.eval(parse(json), "/*/1") must contain(exactly(Node(JInt(2)), Node(JInt(5)), Node(JInt(8))))
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
      JsonPath.eval(parse(json), "/*/1") must contain(exactly(Node(JInt(2)), Node(JInt(5)), Node(JInt(8))))
    }

    "return by property from all object elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  { "a": 10 },
          |  { "a": 11 },
          |]
        """.stripMargin
      JsonPath.eval(parse(json), "/*/a") must contain(exactly(Node("a", JInt(10)), Node("a", JInt(11))))
    }

    "return all array elements" in {
      JsonPath.eval(json, "heroes/*") must contain(exactly(
        Node(JObject(("name", JString("Forrest")), ("surname", JString("Gump")))),
        Node(JObject(("name", JString("Dan")), ("surname", JString("Taylor")))),
        Node(JObject(("name", JString("Jenny")), ("surname", JString("Curan"))))
      ))
    }

    "return a property from all array elements" in {
      JsonPath.eval(json, "heroes/*/name") must contain(exactly(
        Node("name", JString("Forrest")),
        Node("name", JString("Dan")),
        Node("name", JString("Jenny"))
      ))
    }

    "return filtered fields" in {
      JsonPath.eval(json, "heroes/*/name[value.length > 3 && value.startsWith('F')]") must
        contain(exactly(Node("name", JString("Forrest"))))
    }

  }

}
