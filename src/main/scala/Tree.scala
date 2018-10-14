import scala.annotation.tailrec
import scala.math.max
""" The purpose of this project is to try and implement standard methods from other sequence types in the below Binary Search Tree type"""
""" Methods like insert, remove, search, map, toList, filter, reduce"""

sealed abstract class Tree[+T] {
  def insert[T : Ordering](newValue : T) : Tree[T]

  def search[U >: T : Ordering](u : U) : Option[Tree[U]]

  def remove[U >: T : Ordering](u : U) : Tree[T]

  def inOrderPredecessor : Tree[T]

  def inOrderSuccessor : Tree[T]

  def greatestElement : Tree[T]

  def smallestElement : Tree[T]

  def isIdentical[U >: T : Ordering](tree2 : Tree[U]) : Boolean//Boolean

  def depth : Int

  def map[U](f : T => U) : Tree[U]

  def toInOrderList : List[T]

  def toPreOrderList : List[T]

  def toPostOrderList : List[T]

  def filter(f : T=>Boolean ) : Tree[T] = Node(toPostOrderList filter f)

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

  override def remove[U >: T : Ordering](u : U) : Tree[T] = {
    if(implicitly[Ordering[U]].equiv(value,u)) {
      inOrderPredecessor match {
        case Node(v,_,_) => Node(v,left.remove(v),right)
        case End => inOrderSuccessor match {
          case Node(v,_,_) => Node(v,left,right.remove(v))
          case End => End
        }
      }
    } else if( implicitly[Ordering[U]].lt(u,value)) {
      Node(value, left.remove(u), right)
    } else {
      Node(value, left, right.remove(u))
    }
  }

  override def inOrderPredecessor : Tree[T] = left.greatestElement

  override def inOrderSuccessor = right.smallestElement

  override def greatestElement = right match {
    case n : Node[T] => n.greatestElement
    case End => this
  }

  override def smallestElement = left match {
    case n : Node[T] => n.smallestElement
    case End => this
  }

  override def isIdentical[U >: T : Ordering](tree2 : Tree[U]) : Boolean = tree2 match {
    case Node(u, uLeft, uRight) if( implicitly[Ordering[U]].equiv(value,u) ) => (left.isIdentical(uLeft) && right.isIdentical(uRight))
    case _ => false
  }

  override def depth : Int = 1 + max(left.depth, right.depth)

  override def map[U](f: T => U) : Tree[U] = Node(f(value), left map f, right map f)

  override def toInOrderList = (left.toInOrderList :+ value) ++: right.toInOrderList

  override def toPreOrderList = (value +: left.toPreOrderList) ++: right.toPreOrderList

  override def toPostOrderList = (left.toPostOrderList ++: right.toPostOrderList) :+ value
}

case object End extends Tree[Nothing] {

  override def insert[T: Ordering](newValue: T) = Node(newValue)

  override def search[U >: Nothing : Ordering](u : U) : Option[Tree[U]] = None

  override def remove[U >: Nothing : Ordering](u: U) = End

  override def inOrderPredecessor = End

  override def inOrderSuccessor = End

  override def greatestElement = End

  override def smallestElement = End

  override def isIdentical[U >: Nothing : Ordering](tree2 : Tree[U]) : Boolean = tree2 match {
    case End => true
    case _ => false
  }

  override def depth = 0

  override def map[U](f: Nothing => U) = End

  override def toInOrderList = Nil

  override def toPreOrderList = Nil

  override def toPostOrderList = Nil
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


