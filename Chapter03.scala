package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(head, tail) => Cons(head, append(tail, a2))
    }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Whoa nillly")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], head: A) = l match {
    case Nil => sys.error("Whoa nillly")
    case Cons(_, t) => Cons(head, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(List.tail(l), n - 1)
  
  /*
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }
  */

  def dropWhile[A](a: List[A])(f: A => Boolean): List[A] = a match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => a
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Whoa nillly")
    case Cons (_, Nil) => Nil 
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)
  
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)
  
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], acc: B)(f: (B, A) => B): B = l match {
      case Nil => acc
      case Cons(head, tail) => foldLeft(tail, f(acc, head))(f)
  }

  def sumLeft(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def lengthLeft(l: List[Int]) =
    foldLeft(l, 0)((acc, _) => acc + 1)
}