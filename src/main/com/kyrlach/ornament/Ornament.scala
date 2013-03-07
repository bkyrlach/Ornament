package com.kyrlach.ornament

import org.w3c.dom.Node
import org.w3c.dom.NodeList

object Ornament {
  implicit def nodeList2ListNode(nodeList: NodeList): List[Node] = for(i <- (0 until nodeList.getLength).toList) yield { nodeList.item(i) }
  
  def At(rootNodes: List[Node])(fs: List[(String, List[Node] => Unit)]): List[Node] = for(root <- rootNodes) yield { At(root)(fs) }
  
  def At(root: Node)(fs: List[(String, List[Node] => Unit)]): Node = {
    fs foreach { data =>
      data._2(CSSParser.getSelector(data._1)(root))
    }
    root
  }
  

  def Snippet[A, B](root: Node, selector: String, tf: List[Node] => (A, B) => List[Node]): (A, B) => List[Node] = {
    tf(CSSParser.getSelector(selector)(root).map{ node =>
      node.cloneNode(true)
      node
    })    
  }
  
  def Content(content: String): List[Node] => Unit = nodes => for(node <- nodes) {
    val doc = node.getOwnerDocument
    val textContent = doc.createTextNode(content)
    node.appendChild(textContent)
  }
  
  def Content(content: Node): List[Node] => Unit = nodes => for(node <- nodes) {
    node.appendChild(content)
  }
  
  def Content(contentNodes: List[Node]): List[Node] => Unit = nodes => {
    for(node <- nodes;
        content <- contentNodes) {
      node.appendChild(content)
    }
  }
}