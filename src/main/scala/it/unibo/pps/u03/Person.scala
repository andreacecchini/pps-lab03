package u03

import u03.Optionals.*
import Optional.*
import u03.Sequences.*
import Sequence.*

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Person:
  def name(p: Person): String = p match
    case Student(n, _) => n
    case Teacher(n, _) => n

  def course(p: Person): Optional[String] = p match
    case Teacher(_, c) => Just(c)
    case _ => Empty()

  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  def getCourses(people: Sequence[Person]): Sequence[String] =
    flatMap(people)(p => toSequence(course(p)))
