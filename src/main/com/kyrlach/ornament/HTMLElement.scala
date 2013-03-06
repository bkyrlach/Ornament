package com.kyrlach.ornament

trait HTMLElement

case class A(href: String) extends HTMLElement
case class HEAD(elems: List[HTMLElement]) extends HTMLElement
case class BODY(elems: List[HTMLElement]) extends HTMLElement
case class HTML(head: HEAD, body: HTMLElement) extends HTMLElement
