package com.github.unisay.jsontr

import com.github.unisay.jsontr.JsonTr.transform
import net.javacrumbs.jsonunit.JsonAssert.assertJsonEquals
import org.specs2.mutable.Specification

class JsonTrSpec extends Specification {

  val anyJson =
    """
      |{
      |  "field_1": "value 1",
      |  "field_2": {
      |      "field_3" : [ "str", null, 1, 2.0, true, {}, [] ]
      |  }
      |}
    """.stripMargin

  "JsonTr" should {

    "throw exception for invalid template" in {
      transform(anyJson, "[]") must throwA[InvalidTemplateException]
    }

    "produce blank document when no root matched" in {
      transform(anyJson, """ { "a": "b" } """) must be empty
    }

    "match root, replace it with array" in {
      val replacement = """ [ "a", "b", "c" ] """
      val template = s""" { "(match /)": $replacement } """
      assertJsonEquals(replacement, transform(anyJson, template)) must not throwA
    }

    "handle duplicate matches" in {
      val replacement = """ { "a": "b" } """
      val template = s""" { "(match /)": $replacement, "(match /)": []} """
      assertJsonEquals(replacement, transform(anyJson, template)) must not throwA
    }

    "match root, replace it with object" in {
      val replacement = """ { "a": "b" } """
      val template = s""" { "(match /)": $replacement } """
      assertJsonEquals(replacement, transform(anyJson, template)) must not throwA
    }

    "match root, do not replace with bool" in {
      transform(anyJson, """ { "(match /)": true } """) must throwA[InvalidTemplateException]
    }

    "match root, do not replace with string" in {
      val template = """ { "(match /)": "foo" } """
      transform(anyJson, template) must throwA[InvalidTemplateException]
    }

    "match root, do not replace with number" in {
      val template = """ { "(match /)": 1 } """
      transform(anyJson, template) must throwA[InvalidTemplateException]
    }

    "match root, do not replace with null" in {
      val template = """ { "(match /)": null } """
      transform(anyJson, template) must throwA[InvalidTemplateException]
    }

    "match root, copy value-of absolute property path in object" in {
      val source =
        """
          |{
          |  "a": 1,
          |  "b": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": {
          |    "(value-of)" : "/a"
          |  }
          |}
          | """.stripMargin
      val expected =
        """
          |{
          |  "a": 1
          |}
        """.stripMargin
      assertJsonEquals(expected, transform(source, template)) must not throwA
    }.pendingUntilFixed("Implement (value-of)")

    "match root, copy value-of relative property path into object" in {
      val source =
        """
          |{
          |  "a": {
          |    "b": 1
          |  },
          |  "c": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": {
          |    "(value-of)" : "a/b"
          |  }
          |}
        """.stripMargin
      val expected =
        """
          |{
          |  "b": 1
          |}
        """.stripMargin
      assertJsonEquals(expected, transform(source, template)) must not throwA
    }.pendingUntilFixed("Implement (value-of)")

    "match root, copy value-of absolute property path into array" in {
      val source =
        """
          |{
          |  "a": 1,
          |  "b": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": [
          |    "(value-of)" : "/a"
          |  ]
          |}
        """.stripMargin
      assertJsonEquals("[1]", transform(source, template)) must not throwA
    }.pendingUntilFixed("Implement (value-of)")

    "match root, copy value-of relative property path into array" in {
      val source =
        """
          |{
          |  "a": 1,
          |  "b": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": [
          |    "(value-of)" : "a"
          |  ]
          |}
        """.stripMargin
      assertJsonEquals("[1]", transform(source, template)) must not throwA
    }.pendingUntilFixed("Implement (value-of)")

  }

}