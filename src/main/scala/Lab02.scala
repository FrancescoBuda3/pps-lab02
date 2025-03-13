import Lab02.Expr.{Add, Literal, Multiply}
import org.junit.Assert.assertTrue
import org.junit.Test


import scala.annotation.tailrec

object Lab02 extends App:

  // --- Task 1 --- svolto da solo
  println("Hello, Scala!")

  // --- Task 2 --- svolto da solo
  def mult(x: Double, y: Double): Double = x * y
  def curriedMult(x: Double)(y: Double): Double = x * y

  val f = curriedMult(5)
  f(2) // 10.0

  curriedMult(4)(2) // 8.0

  def divide(x: Double, y: Double): Double = x/y
  def curriedDivide(x: Double)(y: Double): Double = x / y

  divide(4,2) // 2.0
  divide(4,0) // Infinity
  divide(0,0) // NaN

  val g = curriedDivide(4)
  f(0) // Infinity

  curriedDivide(0)(0) //NaN

  // --- Task 3 --- svolto da solo

  val positive: Int => String = x => x match
    case n if n >= 0 => "Positive"
    case _ => "Negative"

  println(positive(-4)) // Negative

  def positiveM(x: Int): String = x match
    case n if n >= 0 => "Positive"
    case _ => "Negative"

  println(positiveM(5)) // Positive

  val neg: (String => Boolean) => String => Boolean = p => !p(_)
  val empty: String => Boolean = _ == ""
  val notEmpty = neg(empty)

  println(notEmpty("foo") && !notEmpty("")) // true

  def negM: (String => Boolean) => String => Boolean = p => !p(_)
  def genNeg[A]: (A => Boolean) => A => Boolean = p => !p(_)

  @Test def testWithStrings(): Unit =
    val empty: String => Boolean = _ == ""
    val notEmpty = genNeg(empty)
    assertTrue(notEmpty("foo") && !notEmpty(""))

  @Test def testWithInts(): Unit =
    val isPos: Int => Boolean = _ >= 0
    val isNeg = genNeg(isPos)
    assertTrue(isNeg(-1) && !isNeg(1))

  // --- Task 4 --- svolto da solo

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  println(p1(1)(4)(4) && !p1(4)(3)(3) && !p1(2)(5)(8)) // true

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  println(p2(1, 4, 4) && !p2(4, 3, 3) && !p2(2, 5, 8)) // true

  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  println(p3(1)(4)(4) && !p3(4)(3)(3) && !p3(2)(5)(8)) // true

  def p4(x: Int, y:Int, z:Int): Boolean = x <= y && y == z
  println(p4(1, 4, 4) && !p4(4, 3, 3) && !p4(2, 5, 8)) // true

  // --- Task 5 --- svolto da solo

  val compose: (Int => Int, Int => Int) => Int => Int = (f, g) => x => f(g(x))
  println(compose(_ - 1, _ * 2)(5)) // 9

  def genCompose[A, B, C] (f: B => C, g: A => B): A => C = x => f(g(x))
  println(genCompose((x: Int) => x - 1, (x: Int) => x * 2)(5)) // 9

  // --- Task 6 --- svolto da solo
  def composeThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = genCompose(f, genCompose(g, h))
  println(composeThree((s: String) => s + "!", _.toString, (n: Int) => n * 2)(3)) // "6!"

  // --- Task 7 --- svolto da solo
  def power(base: Double, exponent: Int): Double = exponent match
    case e if e == 1 => base
    case _ => base * power(base, exponent-1)

  println((power(2, 3), power(5, 2))) // (8.0, 25.0)

  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(b: Double, e: Int, acc: Double): Double = e match
      case 1 => acc
      case _ => _power(b, e-1, b*acc)
    _power(base, exponent, base)

  println((powerTail(2, 3), powerTail(5, 2))) // (8.0, 25.0)

  // --- Task 8 --- svolto da solo

  def reverseDigit(num: Int): Int =
    @annotation.tailrec
    def _rev(n: Int, acc: Int, i: Int): Int = n match
      case 0 => acc
      case _ => _rev(n/10, acc*10 + n%10, i+1)
    _rev(num, 0, 0)

  println(reverseDigit(5436)) // 6345

  // --- Task 9 --- svolto da solo

  enum Expr:
    case Literal(value: Int)
    case Add(a: Expr, b: Expr)
    case Multiply(a: Expr, b: Expr)

  def evaluate(e: Expr): Int = e match
    case Expr.Literal(v) => v
    case Expr.Add(a, b) => evaluate(a) + evaluate(b)
    case Expr.Multiply(a, b) => evaluate(a) * evaluate(b)

  println(evaluate(Multiply(Add(Literal(5), Literal(2)), Literal(2)))) //14

  def show(e: Expr): String = e match
    case Expr.Literal(v) => v.toString
    case Expr.Add(a, b) => s"(${show(a)} + ${show(b)})"
    case Multiply(a, b) => s"(${show(a)} * ${show(b)})"

  println(show(Multiply(Add(Literal(5), Literal(2)), Literal(2)))) // ((5 + 2) * 2)

  // --- Task 10 --- svolto da solo
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
    case Optional.Maybe(value) => Optional.Maybe(f(value))
    case _ => Optional.Empty()

  def filter[A](optional: Optional[A], f: A => Boolean): Optional[A] = optional match
    case Optional.Maybe(value) if f(value) => optional
    case _ => Optional.Empty()
    
  println(filter(Optional.Maybe(5), _ > 2)) // Maybe(5)
  println(filter(Optional.Maybe(5), _ > 8)) // Empty()
  println(filter(Optional.Empty(), (x: Int) => x > 2)) // Empty()
  





