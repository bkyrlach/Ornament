package com.kyrlach.ornament

import scala.util.parsing.combinator.RegexParsers

sealed trait Depth
case object AllChildren extends Depth
case object DirectChildren extends Depth

object CSSParser extends RegexParsers {
  import Selectors._
  def nthChild: Parser[List[XMLNode] => Option[XMLNode]] = ":nth-child(" ~> """[^)]+""".r <~ ")" ^^ { count => nodes => nodes.filter(_.isInstanceOf[XMLElement]).drop(count.toInt - 1).headOption } 
  def firstChild: Parser[List[XMLNode] => Option[XMLNode]] = ":first-child" ^^ { ignore => nodes => nodes.filter(_.isInstanceOf[XMLElement]).headOption }
  def lastChild: Parser[List[XMLNode] => Option[XMLNode]] = ":last-child" ^^ { ignore => nodes => nodes.filter(_.isInstanceOf[XMLElement]).lastOption }
  def id(depth: Depth = AllChildren): Parser[XMLNode => List[XMLNode]] = "#" ~> """[a-zA-Z0-9]+""".r ^^ { id => if(depth == AllChildren) (_: XMLNode) \\ idSelector(id) else (_: XMLNode) \ idSelector(id) }
  def element(depth: Depth = AllChildren): Parser[XMLNode => List[XMLNode]] = ("*" | """[a-zA-Z]+""".r) ~ opt(limiter) ^^ { result => ((nodes: List[XMLNode]) => {
    result._2.map(l => l.apply(nodes).map(n => List(n)).getOrElse(Nil)).getOrElse(nodes)
  }) compose ((_: XMLNode) \\ tagSelector(result._1)) } 
  def directDescendants = nonRepeating(AllChildren) ~ ">" ~ nonRepeating(DirectChildren) ^^ { result => result._1._1(_: XMLNode).flatMap(result._2)}
  
  def limiter = firstChild | lastChild | nthChild
  def nonRepeating(depth: Depth = AllChildren): Parser[XMLNode => List[XMLNode]]= id(depth) | element(depth)
  def selectorParts =  directDescendants | nonRepeating()
  def selector = repsep(selectorParts, ",")

  def getSelector(s: String): XMLNode => List[XMLNode] = {
    println(parseAll(selector, s))
    rootNode => parseAll(selector, s).get.flatMap(_(rootNode))
  }
}
