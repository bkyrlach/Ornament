package com.kyrlach.ornament

import org.w3c.dom.Document

class Extractor[A](selector: String, work: List[XMLNode] => Option[A]) {
  def apply(doc: Document): Option[A] = work(CSSParser.getSelector(selector)(doc))
}