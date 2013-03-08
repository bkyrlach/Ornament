package com.kyrlach.ornament

import scala.util.parsing.combinator.RegexParsers
import org.w3c.dom.NodeList
import org.w3c.dom.Node
import org.w3c.dom.Element

object CSSParser extends RegexParsers {
  implicit def nodeList2ListNode(nodeList: NodeList): List[Node] = for(i <- (0 until nodeList.getLength).toList) yield { nodeList.item(i) }
    
  override def skipWhitespace = false
  
  private def searchForAttribute(n: Node, name: String, value: Option[String], negate: Boolean = false, depth: Option[Int]): List[Node] = if(n.getAttributes.getNamedItem(name).getNodeValue == value.get) n :: n.getChildNodes.flatMap(s => searchForAttribute(s, name, value, negate, depth)) else n.getChildNodes.flatMap(s => searchForAttribute(s, name, value, negate, depth)) 
  private def searchForName(n: Node, element: String, depth: Option[Int]): List[Node] = { 
    val matchingElement = n match {
      case e: Element if e.getNodeName == element || element == "*" => Some(e)
      case _ => None
    }
    val rest = depth match {
      case Some(d: Int) => if(d > 0) n.getChildNodes.flatMap(searchForName(_: Node, element, Some(d - 1))) else Nil
      case None => n.getChildNodes.flatMap(searchForName(_: Node, element, None))
    }
    matchingElement.map(_ :: rest).getOrElse(rest)
  }
  private def flatten(n: Node): List[Node] = n :: n.getChildNodes.flatMap(flatten)
  
  def firstChild: Parser[List[Node] => Option[Node]] = ":first-child" ^^ ( ignore => nodes => nodes.filter(_.getNodeType == Node.ELEMENT_NODE).headOption )
  def lastChild: Parser[List[Node] => Option[Node]] = ":last-child" ^^ ( ignore => nodes => nodes.filter(_.getNodeType == Node.ELEMENT_NODE).lastOption )
  def limiter = firstChild | lastChild
  
  def id(depth: Option[Int] = None, ignoreSelf: Boolean = false): Parser[Node => List[Node]] = "#" ~> "[a-zA-Z]+".r ^^ ( id => searchForAttribute(_: Node, "id", Some(id), false, depth) )
  def element(depth: Option[Int] = None, ignoreSelf: Boolean = false): Parser[Node => List[Node]] = ("*" | "[a-zA-Z]+".r) ~ opt(limiter) ^^ { stuff =>
    val selector = searchForName(_: Node, stuff._1, depth)
    println(stuff._2)
    stuff._2.map { limiter =>
      (node: Node) => {
        var nodes = selector(node)
        nodes = if(ignoreSelf) nodes.tail else nodes
        limiter(nodes).map(y => List(y)).getOrElse(Nil)
      }
    }.getOrElse(selector)    
  }
  
  def nonRepeatingParts(depth: Option[Int] = None, ignoreSelf: Boolean = false) = element(depth, ignoreSelf) | id(depth, ignoreSelf)
  def selectorPart = directDescendants | nonRepeatingParts() 

  def directDescendants: Parser[Node => List[Node]] = nonRepeatingParts(None) ~ " > " ~ nonRepeatingParts(Some(1), true) ^^ { parts => parts._1._1(_: Node).flatMap(n => parts._2(n)) }
  
  def selector = repsep(selectorPart, ",")
  
  def getSelector(s: String): Node => List[Node] = rootNode => parseAll(selector, s).get.flatMap(_(rootNode)) 
}
