package com.kyrlach.ornament

import org.w3c.dom.Node
import org.w3c.dom.NodeList
import com.kyrlach.ornament.test.TestTransformation

object Ornament {  
  import CSSParser.nodeList2ListNode
  
  def At(rootNodes: List[Node])(fs: List[(String, List[Node] => Unit)]): List[Node] = for(root <- rootNodes) yield { At(root)(fs) }
  
  def At(root: Node)(fs: List[(String, List[Node] => Unit)]): Node = {
    fs foreach { data =>
      data._2(CSSParser.getSelector(data._1)(root))
    }
    root
  }
  
  def cloneThyself(n: Node): Node = {
    n.cloneNode(true)
    n
  }

  def Snippet[A, B](root: Node, selector: String, tf: (() => List[Node]) => (A, B) => List[Node]): (A, B) => List[Node] = {
    tf(() => CSSParser.getSelector(selector)(root).map{cloneThyself}) 
  }
  
  private def deleteChildren(n: Node): Node = {
    n.getChildNodes.foreach(n.removeChild)
    n
  }
  
  def Content(content: String): List[Node] => Unit = nodes => for(node <- nodes) {
    val doc = node.getOwnerDocument
    val textContent = doc.createTextNode(content)
    deleteChildren(node).appendChild(textContent)
  }
  
  def Content(content: Node): List[Node] => Unit = nodes => for(node <- nodes) {
    node.appendChild(content)
  }
  
  def Content(contentNodes: List[Node]): List[Node] => Unit = nodes => {
    for(node <- nodes;
        content <- contentNodes) {
      println(TestTransformation.nodeString(content))
      node.appendChild(cloneThyself(content))
    }
  }
}