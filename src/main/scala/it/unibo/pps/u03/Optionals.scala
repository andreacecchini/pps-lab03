package u03

import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import Sequence.*

// overall module
object Optionals:

  // type "public" definition, exposing structure
  enum Optional[A]:
    case Just(a: A)
    case Empty() // here parens are needed because of genericity

  // operations (/algorithms)
  object Optional:

    def isEmpty[A](opt: Optional[A]): Boolean = opt match
      case Empty() => true
      case _       => false

    def orElse[A, B >: A](opt: Optional[A], orElse: B): B = opt match
      case Just(a) => a
      case _       => orElse

    def map[A, B](opt: Optional[A])(f: A => B): Optional[B] = opt match
      case Just(a) => Just(f(a))
      case _       => Empty()

    def toSequence[A](opt: Optional[A]): Sequence[A] = orElse(map(opt)(Cons(_, Nil())), Nil())

@main def tryOptionals =
  import Optionals.* // to work with Optionals (to see Optional type)
  import Optional.* // to directly access algorithms

  val s1: Optional[Int] = Just(1)
  val s2: Optional[Int] = Empty()

  println(s1) // Some(1)
  println(isEmpty(s1)) // false
  println(orElse(s1, 0)) // 1
  println(orElse(s2, 0)) // 0
  println(map(s1)(i => "val: " + 1)) // Some("val: 1")
  println(map(s2)(i => "val: " + 1)) // None
