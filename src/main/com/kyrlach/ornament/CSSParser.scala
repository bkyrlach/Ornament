package com.kyrlach.ornament

import scala.util.parsing.combinator.RegexParsers

sealed trait Depth
case object AllChildren extends Depth
case object DirectChildren extends Depth

trait DebugStandardTokenParsers extends RegexParsers {
  class Wrap[+T](name:String,parser:Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      println(name+".apply for token "+first+" at position "+pos+" offset "+offset+" returns "+t)
      t
    }
  }
}

object CSSParser extends DebugStandardTokenParsers {
  import Selectors._
  val firstChildPattern = ":first-child"
  val lastChildPattern = ":last-child"
  val idPattern = "#" ~> """[a-zA-Z0-9]+""".r
  val nthChildPattern = ":nth-child(" ~> """[^)]+""".r <~ ")"
  val classPattern = "." ~> """[a-zA-Z0-9]+""".r
  
  def firstChild: Parser[XMLNode => List[XMLNode]] = new Wrap("firstChildLimiter", firstChildPattern ^^ { result =>
    node => firstChildSelector(node).map(List(_)).getOrElse(Nil)
  })
  
  def firstChild(depth: Depth): Parser[XMLNode => List[XMLNode]] = new Wrap("firstChildSelector", firstChildPattern ^^ { result =>
    depth match {
      case DirectChildren => node => node \ firstChildSelector
      case AllChildren => node => node \\ firstChildSelector
    }
  })
  
  def lastChild: Parser[XMLNode => List[XMLNode]] = new Wrap("lastChildLimiter", lastChildPattern ^^ { result => 
    node => lastChildSelector(node).map(List(_)).getOrElse(Nil)
  })
  def lastChild(depth: Depth): Parser[XMLNode => List[XMLNode]] = new Wrap("lastChildSelector", lastChildPattern ^^ { result => 
    depth match {
      case DirectChildren => node => node \ lastChildSelector
      case AllChildren => node => node \\ lastChildSelector
    }
  })
  
  def nthChild: Parser[XMLNode => List[XMLNode]] = nthChildPattern ^^ { count => 
    node => nthChildSelector(count.toInt)(node).map(List(_)).getOrElse(Nil)
  }
  
  def nthChild(depth: Depth): Parser[XMLNode => List[XMLNode]] = nthChildPattern ^^ { count => 
    println(depth)
    depth match {
      case DirectChildren => node => node \ nthChildSelector(count.toInt)
      case AllChildren => node => node \\ nthChildSelector(count.toInt)
    }
  } 
  
  def id: Parser[XMLNode => List[XMLNode]] = idPattern ^^ { id =>
    node => idSelector(id)(node).map(List(_)).getOrElse(Nil)
  }
  def id(depth: Depth): Parser[XMLNode => List[XMLNode]] = idPattern ^^ { id => 
    depth match {
      case DirectChildren => node => node \ idSelector(id) 
      case AllChildren => node => node \\ idSelector(id)
    }
  }
  
  def clazz: Parser[XMLNode => List[XMLNode]] = classPattern ^^ { name =>
    node => classSelector(name)(node).map(List(_)).getOrElse(Nil)
  }
  
  def clazz(depth: Depth): Parser[XMLNode => List[XMLNode]] = classPattern ^^ { name =>
    depth match {
      case DirectChildren => node => node \ classSelector(name) 
      case AllChildren => node => node \\ classSelector(name)
    }
  }
  def element(depth: Depth): Parser[XMLNode => List[XMLNode]] = new Wrap("element", ("*" | """[a-zA-Z]+""".r) ~ opt(limiter) ^^ { result => 
    result._2 match {
      case Some(f) => depth match {
        case DirectChildren => node => (node \ tagSelector(result._1)).flatMap(f)
        case AllChildren => node => (node \\ tagSelector(result._1)).flatMap(f)
      }
      case None => depth match {
        case DirectChildren => node => node \ tagSelector(result._1)
        case AllChildren => node => node \\ tagSelector(result._1)
      }
    }
  })
  
  def limiter = new Wrap("limiter", id | firstChild | lastChild | nthChild | clazz)
  
  def elements(depth: Depth) = new Wrap("elements", (element(depth) | id(depth) | firstChild(depth) | lastChild(depth) | nthChild(depth) | clazz(depth))*)

  def selectorParts(depth: Depth): Parser[XMLNode => List[XMLNode]] = new Wrap("selectorParts", elements(depth) ~ opt(">" ~> selectorParts(DirectChildren)) ^^ ( result => result._2 match {
    case Some(f) => node => result._1.foldLeft(List(node)){ (acc, f) => acc.flatMap(f) }.flatMap(y => f(y))
    case None => node => result._1.foldLeft(List(node)){ (acc, f) => acc.flatMap(f) }
  }))
  
  def selector = new Wrap("selector", repsep(selectorParts(AllChildren), ","))

  def getSelector(s: String): XMLNode => List[XMLNode] = {
    println(parseAll(selector, s))
    rootNode => parseAll(selector, s).get.flatMap(_(rootNode))
  }
}
