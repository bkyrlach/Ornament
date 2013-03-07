package com.kyrlach.ornament

import org.w3c.dom.Document

case class Transformation(selector: String, work: XMLNode => XMLNode) {
  def apply(document: Document): List[XMLNode] = CSSParser.getSelector(selector)(document).map(work)
} 