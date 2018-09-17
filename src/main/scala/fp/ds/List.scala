package fp.ds

import scala.annotation.tailrec

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
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](ds: List[A], z: B)(f: (A, B) => B): B = ds match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


  def length[A](l: List[A]): Int = foldRight(l, 0)((_, a) => a + 1)

  def sumR(as: List[Int]): Int = foldRight(as, 0)(_ + _)

  def prodR(as: List[Double]): Double = foldRight(as, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil => Nil
    case Cons(_, tl) => tl
  }

  def setHead[A](x: A, ds: List[A]): List[A] = Cons(x, tail(ds))

  def drop[A](l: List[A], n: Int): List[A] = if (n > 0) drop(tail(l), n - 1) else l

  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, tl) => if (p(h)) dropWhile(tl)(p) else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, tl) => Cons(h, init(tl))
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, tl) => foldLeft(tl, f(z, h))(f)
  }

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((a: A, b: B) => f(b, a))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((b: B, a: A) => f(a, b))

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)

  def sumL(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def prodL(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def reverse[A, B](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((a, b) => Cons(b, a))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(foldLeft(a1, Nil: List[A])((a, b) => Cons(b, a)), a2)((a, b) => Cons(b, a))


  def concat[A](as: List[List[A]]): List[A] = as match {
    case Nil => Nil
    case Cons(hd, tl) => append3(hd, concat(tl))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(hd, tl) => Cons(f(hd), map(tl)(f))
  }


}