package com.kyrlach.ornament

import com.codecommit.antixml.Selector
import com.codecommit.antixml.Node
import com.codecommit.antixml.Elem

object Selectors {
  def idSelector(id: String): Selector[Node] = Selector({ 
    case e: Elem if e.attr("id").map(_ == id).getOrElse(false) => e
  })
}