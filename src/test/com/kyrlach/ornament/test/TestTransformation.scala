package com.kyrlach.ornament.test

import javax.xml.parsers.DocumentBuilderFactory
import java.io.File
import com.kyrlach.ornament.Ornament._
import org.w3c.dom.Node
import org.w3c.dom.Element
import com.kyrlach.ornament.CSSParser
import com.kyrlach.ornament.Ornament

object TestTransformation {
  import CSSParser.nodeList2ListNode
  
  def nodeString(n: Node): String = {
    n match {
      case n: Node if n.getNodeType == Node.TEXT_NODE => n.getNodeValue
      case e: Element => "<" + n.getNodeName + (if(n.hasChildNodes) ">" + n.getChildNodes.map(nodeString).foldLeft("")(_ + _)  + "</" + n.getNodeName + ">" else "/>")
      case n: Node => n.getChildNodes.map(nodeString).foldLeft("")(_ + _)
    }
  }
  
  def main(args: Array[String]): Unit = {
    
    val factory = DocumentBuilderFactory.newInstance();
    val builder = factory.newDocumentBuilder()
    val doc = builder.parse(new File("template-test.html"))
    
    println(CSSParser.getSelector("tbody")(doc))
    println(CSSParser.getSelector("tbody > *:first-child")(doc))
    
    val snippet2 = Snippet(doc, "tbody > *:first-child", nf => {(fruit: String, quantity: Int)  =>
      At(nf()) (
        List[(String, List[Node] => Unit)](
          ("tr > *:first-child", Content(fruit)),
          ("tr > *:last-child" , Content(quantity.toString))
        )
      )
    })
    
    val fruitData = Map("a" -> 1, "b" -> 2, "c" -> 3)

    def silly(d: Map[String, Int]): List[Node] => Unit = {
      val mapping = fruitData.flatMap(fruit => snippet2(fruit._1, fruit._2)).toList
      println(mapping.map(nodeString))
      Content(mapping)
    }
    
    //content is defined for a Node, String, List<Node>
    val document = At(doc)(
      List[(String, List[Node] => Unit)](
        ("tbody", silly(fruitData))
      )
    )
    
    println(nodeString(document))

    //(em/defsnippet snippet2 "templates/template1.html" ["tbody > *:first-child"] 
//               [fruit quantity] 
//               ["tr > *:first-child"] (em/content fruit)
//               ["tr > *:last-child"] (em/content (str quantity)))
  }
}