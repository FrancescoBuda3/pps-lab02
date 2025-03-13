import org.junit.*
import org.junit.Assert.*

class NegTest:

  @Test def testWithStrings(): Unit =
    val empty: String => Boolean = _ == ""
    val notEmpty = neg(empty)
    assertTrue(notEmpty("foo") && !notEmpty(""))

  @Test def testWithInts(): Unit =
    val isPos: Int => Boolean = _ >= 0
    val isNeg = neg(isPos)
    assertTrue(isNeg(-1) && !isNeg(1))
