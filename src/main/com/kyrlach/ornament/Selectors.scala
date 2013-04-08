package com.kyrlach.ornament

case class Selector[A](f: PartialFunction[XMLNode, A]) {
  def apply(node: XMLNode): Option[A] = if(f.isDefinedAt(node)) Some(f(node)) else None
}

object Selectors {
  def idSelector(id: String): Selector[XMLNode] = Selector({ 
    case e: XMLElement if e.attributes.get("id").map(_ == id).getOrElse(false) => e
  })
  def classSelector(name: String): Selector[XMLNode] = Selector({ 
    case e: XMLElement if e.attributes.get("class").map(_.split(" ").contains(name)).getOrElse(false) => e
  })
  def tagSelector(tagName: String): Selector[XMLNode] = Selector({
    case e: XMLElement if ((e.name == tagName) || tagName == "*") => e
  })
  def nthChildSelector(n: Int): Selector[XMLNode] = Selector({
    case e: XMLElement if (e.parentNode.flatMap(parent => parent.children.filter(_.isInstanceOf[XMLElement]).drop(n - 1).headOption).map(_.uuid == e.uuid).getOrElse(false)) => e
  })
  val firstChildSelector = Selector({
    //TODO FIX THIS HORRIBLE HACK!!!!
    case e: XMLElement if (e.parentNode.flatMap(parent => parent.children.filter(_.isInstanceOf[XMLElement]).headOption).map(_.uuid == e.uuid).getOrElse(false)) => e
  })
  val lastChildSelector = Selector({
    case e: XMLElement if (e.parentNode.flatMap(parent => parent.children.filter(_.isInstanceOf[XMLElement]).lastOption).map(_.uuid == e.uuid).getOrElse(false)) => e
  })
}