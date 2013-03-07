package com.kyrlach.ornament

import scala.util.parsing.combinator.RegexParsers
import org.w3c.dom.NodeList
import org.w3c.dom.Document
import javax.xml.xpath.XPathFactory
import javax.xml.xpath.XPathConstants
import org.w3c.dom.Node

abstract class Selector(val xpath: String)
case class ElementSelector(override val xpath: String) extends Selector(xpath)
case class ElementById(override val xpath: String) extends Selector(xpath)
case class Path(override val xpath: String) extends Selector(xpath)

object CSSParser extends RegexParsers {
  override def skipWhitespace = false  
  
  def firstChild: Parser[String] = ":first-child" ^^ ( ignore => "[1]" )
  def lastChild: Parser[String] = ":last-child" ^^ ( ignore => "[last()]" )
  
  def limiters = firstChild | lastChild
  
  def attribute: Parser[String] = "[" ~> """([a-zA-Z]+)""".r ~ "=" ~ """([a-zA-Z]+)""".r <~ "]" ^^ ( nameValue => "@" + nameValue._1._1 + "='" + nameValue._2 + "'")
  def attributes: Parser[String] = rep(attribute) ^^ { attrs => if(attrs.isEmpty) "" else "[" + attrs.foldLeft("")((attrString, attr) => attrString + attr + " and ").dropRight(5) + "]" }
  def element: Parser[ElementSelector] = ("*" | """([a-zA-Z]+)""".r) ~ opt(attributes) ~ opt(limiters) ^^ ( result =>  ElementSelector(result._1._1 + result._1._2.getOrElse("") + result._2.getOrElse("") )) 

  def byId: Parser[ElementById] = "#" ~> """([^ ]+)""".r ^^ { id => ElementById("*[@id='" + id + "']") }
  
  def nonRepeat: Parser[Selector] = byId | element
  
  def selectorPart: Parser[Selector] = descendants | directDescendants | nonRepeat
  
  def descendants: Parser[Path] = nonRepeat ~ " " ~ nonRepeat ^^ { parts => Path(parts._1._1.xpath + "//" + parts._2.xpath) }
  
  def directDescendants: Parser[Path] = nonRepeat ~ " > " ~ nonRepeat ^^ { parts => Path(parts._1._1.xpath +"/" + parts._2.xpath)  }
  
  def selector: Parser[List[Selector]] = repsep(selectorPart, ",") ^^ { selectors => selectors.map{
    case e: ElementSelector => Path("//" + e.xpath)
    case eid: ElementById => Path("//" + eid.xpath)
    case p: Path => Path("//" + p.xpath)
  }}
  
  def getSelector(s: String): Node => List[Node] = {
    import Ornament.nodeList2ListNode
    
    println("Selector: " + s)
    //println(parseAll(selector, s))
    val selectors = parseAll(selector, s).get
    rootElement => (selectors.flatMap{ selector =>
      println("XPath: " + selector.xpath)
      val xpath = XPathFactory.newInstance().newXPath()
      val expr = xpath.compile(selector.xpath)
      
      expr.evaluate(rootElement, XPathConstants.NODESET).asInstanceOf[NodeList]      
    })
  }
}
