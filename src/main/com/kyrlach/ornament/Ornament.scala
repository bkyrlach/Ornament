package com.kyrlach.ornament

import java.util.UUID

object Ornament {
  def snippet[A](root: XMLNode, selector: String, transform: List[XMLNode] => A => List[XMLNode]): A => List[XMLNode] = {
    transform(CSSParser.getSelector(selector)(root).map(_.copy(uuid = UUID.randomUUID)))
  }
  def snippet[A, B](root: XMLNode, selector: String, transform: List[XMLNode] => (A, B) => List[XMLNode]): (A, B) => List[XMLNode] = {
    transform(CSSParser.getSelector(selector)(root).map(_.copy(uuid = UUID.randomUUID)))
  }
  def snippet[A, B, C](root: XMLNode, selector: String, transform: List[XMLNode] => (A, B, C) => List[XMLNode]): (A, B, C) => List[XMLNode] = {
    transform(CSSParser.getSelector(selector)(root).map(_.copy(uuid = UUID.randomUUID)))
  }
  def snippet[A, B, C, D](root: XMLNode, selector: String, transform: List[XMLNode] => (A, B, C, D) => List[XMLNode]): (A, B, C, D) => List[XMLNode] = {
    transform(CSSParser.getSelector(selector)(root).map(_.copy(uuid = UUID.randomUUID)))
  }
  def at(node: XMLNode, transforms: (String, List[XMLNode] => List[XMLNode])*): XMLNode = transforms.toList.flatMap(tf => tf._2(CSSParser.getSelector(tf._1)(node))).foldLeft(node)((acc, n) => acc.replaceNode(n))

  def at(nodes: List[XMLNode], transforms: (String, List[XMLNode] => List[XMLNode])*): List[XMLNode] = nodes.map(node => at(node, transforms: _*))

  def content(c: String): List[XMLNode] => List[XMLNode] = nodes => nodes.map(_.replaceChildren(List(new TextNode(None, None, c, UUID.randomUUID))))
  def content(c: XMLNode): List[XMLNode] => List[XMLNode] = nodes => nodes.map(_.replaceChildren(c))
  def content(c: List[XMLNode]): List[XMLNode] => List[XMLNode] = nodes => nodes.map(_.replaceChildren(c))
}