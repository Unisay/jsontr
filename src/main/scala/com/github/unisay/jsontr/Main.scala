package com.github.unisay.jsontr

object Main {

  def main(args: Array[String]) {
    val source = scala.io.Source.fromFile(args(0)).mkString
    val template = scala.io.Source.fromFile(args(1)).mkString
    print(JsonTr.transform(source, template))
  }

}
