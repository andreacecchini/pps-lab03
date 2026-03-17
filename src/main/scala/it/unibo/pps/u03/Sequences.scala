package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.{map as _, *}
import u03.Sequences.Sequence.*

import scala.annotation.tailrec

object Sequences:

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (Nil(), _) => Nil()
      case (s, n) if n == 0 => s
      case (Cons(_, t), n) => skip(t)(n - 1)

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), _) => Nil()
      case (_, Nil()) => Nil()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (_, Nil()) => s1
      case (Nil(), _) => s2
      case (Cons(h1, t1), _) => Cons(h1, concat(t1, s2))

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def loop(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Nil() => acc
        case Cons(h, t) => loop(t, Cons(h, acc))

      loop(s, Nil())

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = s match
      case Nil() => Empty()
      case Cons(h, t) => Just(math.min(h, min(t) match
        case Just(a) => a
        case Empty() => h))

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def loop(s: Sequence[A], acc: Sequence[A], idx: Int): Sequence[A] = (s, idx) match
        case (Nil(), _) => acc
        case (Cons(h, t), idx) if idx % 2 == 0 => loop(t, concat(acc, Cons(h, Nil())), idx + 1)
        case (Cons(_, t), idx) => loop(t, acc, idx + 1)

      loop(s, Nil(), 0)

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Nil() => false
      case Cons(h, t) => h == elem || contains(t)(elem)

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, t) if contains(distinct(t))(h) => Cons(h, filter(distinct(t))(_ != h))
      case Cons(h, t) => Cons(h, distinct(t))


    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      @tailrec
      def loop(remaining: Sequence[A],
               currentGroup: Sequence[A],
               result: Sequence[Sequence[A]]): Sequence[Sequence[A]]
      = (remaining, currentGroup) match
        case (Nil(), Nil()) => result
        case (Nil(), Cons(_, _)) => concat(result, Cons(currentGroup, Nil()))
        case (Cons(h, t), Cons(h1, _)) if h != h1 => loop(t, Cons(h, Nil()), concat(result, Cons(currentGroup, Nil())))
        case (Cons(h, t), _) => loop(t, Cons(h, currentGroup), result)

      loop(s, Nil(), Nil())

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      (filter(s)(el => pred(el)), filter(s)(el => !pred(el)))

@main def trySequences(): Unit =
  import Sequences.*
  val l = Cons(10, Cons(20, Cons(30, Nil())))
  println(sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
