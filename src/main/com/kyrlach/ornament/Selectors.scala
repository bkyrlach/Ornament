package com.kyrlach.ornament

case class Selector[A](f: PartialFunction[XMLNode, A]) {
  def apply(node: XMLNode): Option[A] = if(f.isDefinedAt(node)) Some(f(node)) else None
}

object Selectors {
  def idSelector(id: String): Selector[XMLNode] = Selector({ 
    case e: XMLElement if e.attributes.get("id").map(_ == id).getOrElse(false) => e
  })
  def tagSelector(tagName: String): Selector[XMLNode] = Selector({
    case e: XMLElement if ((e.name == tagName) || tagName == "*") => e
  })
}