package com.kyrlach.ornament.test

import javax.xml.parsers.DocumentBuilderFactory
import java.io.File
import com.kyrlach.ornament.Ornament
import org.w3c.dom.Node
import org.w3c.dom.Element

object TestTransformation {
  import Ornament._
  
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
    
    val snippet2 = Snippet(doc, "tbody > *:first-child", nodes => (fruit: String, quantity: Int)  =>
      At(nodes) (
        List[(String, List[Node] => Unit)](
          ("tr > *:first-child", Content(fruit)),
          ("tr > *:last-child" , Content(quantity.toString))
        )
      )
    )
    
    val fruitData = Map("a" -> 1, "b" -> 2, "c" -> 3)


    //content is defined for a Node, String, List<Node>
    val document = At(doc)(
      List[(String, List[Node] => Unit)](
        ("tbody", Content(fruitData.flatMap(fruit => snippet2(fruit._1, fruit._2)).toList))
      )
    )
    
    println(nodeString(document))

    //(em/defsnippet snippet2 "templates/template1.html" ["tbody > *:first-child"] 
//               [fruit quantity] 
//               ["tr > *:first-child"] (em/content fruit)
//               ["tr > *:last-child"] (em/content (str quantity)))
  }
}