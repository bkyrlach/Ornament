package com.kyrlach.ornament

import org.w3c.dom.Document
import org.w3c.dom.Node
import org.w3c.dom.NodeList
import org.w3c.dom.Element
import org.w3c.dom.NamedNodeMap

abstract class XMLNode(val n: Node) {
  def cloneNode = HTMLElement(n.cloneNode(true))
  
  implicit val doc = n.getOwnerDocument
  
  def appendChild(node: XMLNode): Unit = {
    println(node)
    println(this.n)
    n.appendChild(node.n)
  }
  
  def content: String = children.map(_.toString).foldLeft("")(_ + _)
  def content_=(text: String) = {
    deleteChildren
    appendChild(new TEXT_NODE(text))
  }
  
  private def deleteChildren: Unit = HTMLElement.mapNodes(n.getChildNodes)(identity).foreach(n.removeChild)
  
  def children: List[XMLNode] = HTMLElement(n.getChildNodes())
  def children_=(value: List[XMLNode]) = {
    deleteChildren
    value.foreach(xmlNode => n.appendChild(xmlNode.n))
  }
  
  private def attrStrBuilder(attrs: NamedNodeMap): String = (for(i <- (0 until attrs.getLength()).toList) yield { 
    val attr = attrs.item(i)
    (attr.getNodeName(), attr.getNodeValue())
  }).foldLeft("")((str, attr) => str + " " + attr._1 + "='" + attr._2 + "'")
  
  private def nodeStringBuilder(n: Node): String = n match {
    case n: Node if n.getNodeType == Node.TEXT_NODE => n.getNodeValue
    case n: Node if n.getNodeType == Node.DOCUMENT_NODE => "<xml>" + (if(n.hasChildNodes) ">" + HTMLElement.mapNodes(n.getChildNodes)(nodeStringBuilder).reduceLeft(_ + _) + "</xml>" else "/>")
    case e: Element => "<" + e.getNodeName + attrStrBuilder(e.getAttributes) + (if(e.hasChildNodes) ">" + HTMLElement.mapNodes(e.getChildNodes)(nodeStringBuilder).reduceLeft(_ + _) + "</" + e.getNodeName + ">" else "/>")     
  }
  
  override def toString(): String = nodeStringBuilder(n)
}

class DOCUMENT(document: Node) extends XMLNode(document)

class TEXT_NODE(textNode: Node) extends XMLNode(textNode) {
  def this(text: String)(implicit doc: Document) = {
    this(doc.createTextNode(text))
  }
}

abstract class HTMLElement(val e: Element) extends XMLNode(e) {
  def firstChildElement = children.filter(_.isInstanceOf[HTMLElement]).headOption 
  def lastChildElement = children.filter(_.isInstanceOf[HTMLElement]).lastOption
}

class A(anchor: Element) extends HTMLElement(anchor) {
  def this(href: String, text: String)(implicit doc: Document) = {
    this(doc.createElement("a"))
    val textNode = doc.createTextNode(text)
    anchor.appendChild(textNode)
    val hrefAttr = doc.createAttribute("href")
    hrefAttr.setValue(href)
    anchor.getAttributes().setNamedItem(hrefAttr)
  }
}

trait Valued {
  self: HTMLElement =>
  def value: String = self.e.getAttribute("value")
  def value_=(value: String) = self.e.setAttribute("value", value)
}

class INPUT(input: Element) extends HTMLElement(input) with Valued

class OPTION(option: Element) extends HTMLElement(option) with Valued

class TR(tr: Element) extends HTMLElement(tr)
class TD(td: Element) extends HTMLElement(td)
class TABLE(table: Element) extends HTMLElement(table)
class THEAD(thead: Element) extends HTMLElement(thead)
class TBODY(tbody: Element) extends HTMLElement(tbody)

object HTMLElement {
  def mapNodes[A](nodes: NodeList)(f: Node => A): List[A] = for(i <- (0 until nodes.getLength).toList) yield { f(nodes.item(i)) }
  
  def apply(nodes: NodeList): List[XMLNode] = mapNodes(nodes)(nodeMatch)
  def apply(node: Node): XMLNode = nodeMatch(node)
  
  val nodeMatch: Node => XMLNode = {
    case n: Node if n.getNodeType == Node.TEXT_NODE => new TEXT_NODE(n)
    case n: Node if n.getNodeType == Node.DOCUMENT_NODE => new DOCUMENT(n)
    case e: Element if e.getNodeName == "input" => new INPUT(e)
    case e: Element if e.getNodeName == "option" => new OPTION(e)
    case e: Element if e.getNodeName == "tr" => new TR(e)
    case e: Element if e.getNodeName == "td" => new TD(e)
    case e: Element if e.getNodeName == "table" => new TABLE(e)
    case e: Element if e.getNodeName == "thead" => new THEAD(e)
    case e: Element if e.getNodeName == "tbody" => new TBODY(e)
  }
}