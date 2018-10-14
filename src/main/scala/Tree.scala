import scala.annotation.tailrec
""" The purpose of this project is to try and implement standard methods from other sequence types in the below Binary Search Tree type"""
""" Methods like insert, remove, search, map, toList, filter, reduce"""

sealed abstract class Tree[+T] {
  def insert[T : Ordering](newValue : T) : Tree[T]

  def search[U >: T : Ordering](u : U) : Option[Tree[U]]

  def isIdentical[U >: T : Ordering](tree2 : Tree[U]) : Boolean//Boolean

}

case class Node[+T](value : T, left : Tree[T], right : Tree[T]) extends Tree[T] {

  override def insert[T : Ordering](newValue : T) : Tree[T] = {
    if(implicitly[Ordering[T]].lteq(newValue, value)) {
      Node(value, left.insert(newValue), right)
    } else {
      Node(value, left, right.insert(newValue))
    }
  }

  override def search[U >: T : Ordering](u : U) : Option[Tree[U]] = {
    if( implicitly[Ordering[U]].equiv(value, u) ) {
      Some(this)
    } else if( implicitly[Ordering[U]].lt(u,value) ) {
      left.search(u)
    } else {
      right.search(u)
    }
  }

  override def isIdentical[U >: T : Ordering](tree2 : Tree[U]) : Boolean = tree2 match {
    case Node(u, uLeft, uRight) if( implicitly[Ordering[U]].equiv(value,u) ) => (left.isIdentical(uLeft) && right.isIdentical(uRight))
    case _ => false
  }

}

case object End extends Tree[Nothing] {

  override def insert[T: Ordering](newValue: T) = Node(newValue)

  override def search[U >: Nothing : Ordering](u : U) : Option[Tree[U]] = None

  override def isIdentical[U >: Nothing : Ordering](tree2 : Tree[U]) : Boolean = tree2 match {
    case End => true
    case _ => false
  }
}

object Node {
  def apply[T](value : T) = new Node(value, End, End)

  def apply[T](tl : List[T]) = {

    @tailrec
    def inner(root : Tree[T], tl : List[T]) : Tree[T] = tl match {
      case head :: tail => inner(root.insert(head), tail)
      case Nil => root
    }
    inner(End,tl)
  }

}

object MyApp extends App {



}


