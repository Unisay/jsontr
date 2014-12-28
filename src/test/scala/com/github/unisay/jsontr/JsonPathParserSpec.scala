package com.github.unisay.jsontr

import com.github.unisay.jsontr.JsonPathParser.parse
import org.specs2.mutable.Specification

class JsonPathParserSpec extends Specification {

  "JsonPathCombinatorParser.parse" should {

    "require a non-null path" in {
      parse(null) must throwA[IllegalArgumentException]
    } 

    "return empty path for empty string" in {
      parse("") must be equalTo Nil
    }

    "return root path for /" in {
      parse(" / ") must contain(exactly[JsonPathStep](Root()))
    }

    "space-separated / into one" in {
      parse("/ /") must throwA[Exception]
    }

    "return absolute root property path" in {
      parse("/prop") must containTheSameElementsAs(List(Root(), Prop("prop")))
    }

    "trim whitespace for property" in {
      parse("/  prop  ") must containTheSameElementsAs(List(Root(), Prop("prop")))
    }

    "trim whitespace for index" in {
      parse("/  1  ") must containTheSameElementsAs(List(Root(), Index(1)))
    }

    "return absolute element path" in {
      parse("/1") must contain(exactly[JsonPathStep](Root(), Index(1)))
    }

    "return relative property path" in {
      parse("prop") must contain(exactly[JsonPathStep](Prop("prop")))
    }

    "return relative element path" in {
      parse("1") must contain(exactly[JsonPathStep](Index(1)))
    }

    "return absolute nested property path" in {
      parse("/a/b/c") must contain(exactly[JsonPathStep](Root(), Prop("a"), Prop("b"), Prop("c")))
    }

    "return relative nested property path" in {
      parse("a/b/c") must contain(exactly[JsonPathStep](Prop("a"), Prop("b"), Prop("c")))
    }

    "return absolute nested mixed path" in {
      parse("/a/1/2") must contain(exactly[JsonPathStep](Root(), Prop("a"), Index(1), Index(2)))
    }

    "return relative nested mixed path" in {
      parse("a/1/2") must contain(exactly[JsonPathStep](Prop("a"), Index(1), Index(2)))
    }

    "return path containing *" in {
      parse("a/*/c") must contain(exactly[JsonPathStep](Prop("a"), All(), Prop("c")))
    }

    "return path with empty predicate" in {
      parse("a[]") must throwA[RuntimeException]
    }

    "return path with predicates" in {
      def stepMatcher(name: String)(step: JsonPathStep) = step match {
        case Prop(`name`, Some(_)) => true
        case _ => false
      }
      parse("a[p1 < p2 && p3]/b/c[p2.name]") must contain(exactly[JsonPathStep](
        stepMatcher("a") _, Prop("b", None), stepMatcher("c") _))
    }

  }
}
