package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Person
import Person.*
import u03.Sequences.Sequence
import Sequence.*

class PersonTest:
  val me: Person = Student("Andrea Cecchini", 23)
  val viroli: Person = Teacher("Mirko Viroli", "PPS")
  val aguzzi: Person = Teacher("Gianluca Aguzzi", "PPS")
  val ricci: Person = Teacher("Alessandro Ricci", "PCD")

  @Test def testCourses(): Unit =
    val people: Sequence[Person] = Cons(me, Cons(viroli, Cons(aguzzi, Cons(ricci, Nil()))))
    assertEquals(Cons("PPS", Cons("PPS", Cons("PCD", Nil()))),
      getCourses(people))
