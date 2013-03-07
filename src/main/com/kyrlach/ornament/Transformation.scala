package com.kyrlach.ornament

import org.w3c.dom.Document

case class Transformation(selector: String, work: HTMLElement => HTMLElement) {
  def apply(document: Document): List[HTMLElement] = CSSParser.getSelector(selector)(document).map(work)
} 