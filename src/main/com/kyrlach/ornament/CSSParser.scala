package com.kyrlach.ornament

import scala.util.parsing.combinator.RegexParsers
import com.codecommit.antixml.Node
import com.codecommit.antixml.Zipper
import com.codecommit.antixml.Group

object CSSParser extends RegexParsers {
  import Selectors._
  
  def id: Parser[Node => Group[Node]] = "#" ~> """[a-zA-Z0-9]+""".r ^^ { id => node => node match {
    case e: com.codecommit.antixml.Elem => e \\ idSelector(id)
    case _ => Group()
  }}
  
  def nonRepeating = id
  
  def selector = repsep(nonRepeating, ",")

  def getSelector(s: String): Node => List[Node] = rootNode => parseAll(selector, s).get.flatMap(_(rootNode)) 
}
