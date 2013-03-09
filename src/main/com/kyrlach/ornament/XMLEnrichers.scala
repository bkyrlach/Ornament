package com.kyrlach.ornament

import java.util.UUID
import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import org.w3c.dom.Text
import org.w3c.dom.NodeList
import org.w3c.dom.NamedNodeMap

object XMLEnricher {
  implicit def nodeList2ListNode(nl: NodeList): List[Node] = for(i <- (0 until nl.getLength).toList) yield { nl.item(i) }
  
  implicit def namedNodeMap2AttrMap(nnm: NamedNodeMap): Map[String, String] = (for(i <- (0 until nnm.getLength).toList) yield { (nnm.item(i).getNodeName, nnm.item(i).getNodeValue) }).toMap
  
  def apply(d: Document): XMLDocument = new XMLDocument(None, None, Nil, UUID.randomUUID).appendChildren(d.getChildNodes.map(wrapNode)).asInstanceOf[XMLDocument]
  
  private[ornament] def wrapNode(n: Node): XMLNode = n match {
    case d: Document => new XMLDocument(None, None, Nil, UUID.randomUUID)
    case e: Element => new XMLElement(None, None, e.getNodeName, e.getAttributes, Nil, UUID.randomUUID).appendChildren(e.getChildNodes.map(wrapNode))
    case t: Text => new TextNode(None, None, t.getNodeValue, UUID.randomUUID)
  }
}

abstract class XMLNode private[ornament](val ownerDocument: Option[XMLDocument], val parentNode: Option[XMLNode], val children: List[XMLNode], private[ornament] val uuid: UUID) {  
  def \[A](selector: Selector[A]): List[A] = children.flatMap{selector.apply}
  def \\[A](selector: Selector[A]): List[A] = \(selector) ::: children.flatMap(_\\(selector))
  private[ornament] def copy(ownerDocument: Option[XMLDocument] = this.ownerDocument, parentNode: Option[XMLNode] = this.parentNode, children: List[XMLNode] = this.children, uuid: UUID = this.uuid): XMLNode = this match {
    case d: XMLDocument => d.cloneNode(ownerDocument = ownerDocument, parentNode = parentNode, children = children, uuid = uuid)
    case t: TextNode => t.cloneNode(ownerDocument = ownerDocument, parentNode = parentNode,uuid = uuid)
    case e: XMLElement => e.cloneNode(ownerDocument = ownerDocument, parentNode = parentNode, children = children, uuid = uuid)
  }
  
  private[ornament] def appendChild(c: XMLNode): XMLNode = copy(children = this.children :+ (c.copy(parentNode = Some(this), ownerDocument = (if(this.isInstanceOf[XMLDocument]) Some(this.asInstanceOf[XMLDocument]) else ownerDocument))))  
  private[ornament] def appendChildren(children: List[XMLNode]): XMLNode = copy(children = this.children ::: children.map(_.copy()))

  private[ornament] def replaceChildren(c: XMLNode): XMLNode = copy(children = Nil).appendChild(c)
  private[ornament] def replaceChildren(children: List[XMLNode]): XMLNode = copy(children = Nil).appendChildren(children)
  
  private[ornament] def replaceNode(n: XMLNode): XMLNode = if(uuid == n.uuid) n else this.copy(children = children.map(_.replaceNode(n)))
}

class TextNode private[ornament](override val ownerDocument: Option[XMLDocument], override val parentNode: Option[XMLNode], val text: String, uuid: UUID) extends XMLNode(ownerDocument, parentNode, Nil, uuid) {
  override def toString(): String = text
  private[ornament] def cloneNode(ownerDocument: Option[XMLDocument] = this.ownerDocument, parentNode: Option[XMLNode] = this.parentNode, text: String = text, uuid: UUID = uuid): TextNode = new TextNode(ownerDocument, parentNode, text, uuid) 
}

class XMLDocument private[ornament](override val ownerDocument: Option[XMLDocument] = None, override val parentNode: Option[XMLNode] = None, override val children: List[XMLNode] = Nil, uuid: UUID) extends XMLNode(ownerDocument, parentNode, children, uuid) {
  override def toString(): String = "#document: \n" + children.foldLeft("")(_ + _.toString)
  private[ornament] def cloneNode(ownerDocument: Option[XMLDocument] = this.ownerDocument, parentNode: Option[XMLNode] = this.parentNode, children: List[XMLNode] = children, uuid: UUID = uuid): XMLDocument = new XMLDocument(ownerDocument, parentNode, children, uuid)
}

class XMLElement private[ornament](override val ownerDocument: Option[XMLDocument], override val parentNode: Option[XMLNode], val name: String, val attributes: Map[String, String] = Map(), override val children: List[XMLNode] = Nil, uuid: UUID) extends XMLNode(ownerDocument, parentNode, children, uuid) { 
  override def toString(): String = "<" + name + attributes.foldLeft(" ")((str, attr) => str + attr._1 + "='" + attr._2 + "' ") + (if(children == Nil) "/>" else ">" + children.foldLeft("")(_ + _.toString) + "</" + name + ">")
  private[ornament] def cloneNode(ownerDocument: Option[XMLDocument] = this.ownerDocument, parentNode: Option[XMLNode] = this.parentNode, name: String = name, attributes: Map[String, String] = attributes, children: List[XMLNode] = children, uuid: UUID = uuid): XMLElement = new XMLElement(ownerDocument, parentNode, name, attributes, children, uuid)
}