package com.kyrlach.ornament

import org.w3c.dom.Document
import org.w3c.dom.Node
import org.w3c.dom.NodeList
import org.w3c.dom.Element
import org.w3c.dom.NamedNodeMap

abstract class XMLNode(val n: Node)

abstract class HTMLElement(val e: Element) extends XMLNode(e) {
  
  def children: List[XMLNode] = HTMLElement(e.getChildNodes())
  def children_=(value: List[XMLNode]) = {
    HTMLElement.mapNodes(e.getChildNodes)(identity).foreach(e.removeChild)
    value.foreach(xmlNode => e.appendChild(xmlNode.n))
  } 
  
  private def attrStrBuilder(attrs: NamedNodeMap): String = (for(i <- (0 until attrs.getLength()).toList) yield { 
    val attr = attrs.item(i)
    (attr.getNodeName(), attr.getNodeValue())
  }).foldLeft("")((str, attr) => str + " " + attr._1 + "='" + attr._2 + "'")
  
  private def nodeStringBuilder(n: Node): String = n match {
    case n: Node if n.getNodeType == Node.TEXT_NODE => n.getNodeValue
    case e: Element => "<" + e.getNodeName + attrStrBuilder(e.getAttributes) + (if(e.hasChildNodes()) ">" + HTMLElement.mapNodes(e.getChildNodes)(nodeStringBuilder).reduceLeft(_ + _) + "</" + e.getNodeName + ">" else "/>") 
    case _ => "Oops"
  }
  
  override def toString(): String = nodeStringBuilder(e)
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

object HTMLElement {
  def mapNodes[A](nodes: NodeList)(f: Node => A): List[A] = for(i <- (0 until nodes.getLength).toList) yield { f(nodes.item(i)) }
  
  def apply(nodes: NodeList): List[HTMLElement] = mapNodes(nodes) {
    case e: Element if e.getNodeName == "input" => new INPUT(e)
    case e: Element if e.getNodeName == "option" => new OPTION(e)
  }
}