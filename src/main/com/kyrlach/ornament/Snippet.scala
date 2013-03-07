package com.kyrlach.ornament

import org.w3c.dom.Document

class Snippet1[A](selector: String, f: XMLNode => A => XMLNode) {
  def apply(a: A)(implicit doc: Document): List[XMLNode] = CSSParser.getSelector(selector)(doc).map(f(_)(a))
}

class Snippet2[A, B](selector: String, f: XMLNode => (A, B) => XMLNode) {
  def apply(a: A, b: B)(implicit doc: Document): List[XMLNode] = CSSParser.getSelector(selector)(doc).map(f(_)(a, b))
}

object Snippet {
  def apply[A](selector: String, f: XMLNode => A => XMLNode): Snippet1[A] = new Snippet1(selector, f)
  def apply[A, B](selector: String, f: XMLNode => (A, B) => XMLNode): Snippet2[A, B] = new Snippet2(selector, f)
}