import org.scalatest.funsuite.AnyFunSuite
import ru.ifmo.backend_2021._

class Tests extends AnyFunSuite {
  test("Natural numbers") {
    def toNat(i: Int): Nat = if (i <= 0) Z else Succ(toNat(i - 1))

    val five = toNat(5)
    val four = toNat(4)
    val three = toNat(3)
    val two = toNat(2)
    val one = toNat(1)
    val zero = toNat(0)
    val num = Nat.natNum
    assertResult(one)(num.+(zero, one))
    assertResult(three)(num.+(one, two))
    assertResult(five)(num.+(two, three))

    assertResult(two)(num.-(five, three))
    assertResult(zero)(num.-(four, four))
    assertResult(zero)(num.-(one, five))

    assertResult(zero)(num.*(zero, two))
    assertResult(four)(num.*(two, two))
    assertResult(five)(num.*(five, one))

    val eq = Nat.natEq
    assert(eq.eq(five, five))
    assert(!eq.eq(five, four))
    assert(eq.eq(zero, num.*(zero, two)))
  }

  test("Tree base") {
    import Tree._
    val emptyTree = fromList(List())
    assert(isEmpty(emptyTree))
    val testTree = fromList(List(4, 1, 3, 2))
    assert(!isEmpty(testTree))
    assert(contains(1)(testTree))
    assert(contains(2)(testTree))
    assert(contains(3)(testTree))
    assert(contains(4)(testTree))
    val testTree2 = insert(2)(testTree)
    assert(contains(2)(testTree2))
    val testTree3 = delete(2)(testTree2)
    assert(contains(2)(testTree3))
    val testTree4 = delete(2)(testTree3)
    assert(!contains(2)(testTree4))
  }

  test("Tree Foldable") {
    import Tree._
    val laws = foldable.foldableLaw
    assert(laws.rightConsistent(insert(1)(insert(3)(insert(4)(insert(5)(fromList(List(2))))))))

    val list = List(3, 1, 2, 4)
    assertResult(list.sorted)(foldable.toList(fromList(list)))
  }

  test("NonEmpty") {
    import NonEmpty._
    val testNonEmpty = NonEmpty(1, List(2, 3, 4, 5))

    val foldableLaws = foldable.foldableLaw
    assert(foldableLaws.rightConsistent(testNonEmpty))

    val functorLaws = functor.functorLaws
    assert(functorLaws.identity(testNonEmpty))
    assert(functorLaws.composite[Int, Option[Int], Int](testNonEmpty, x => if (x <= 2) None else Some(x), _.getOrElse(0)))
    assertResult(NonEmpty(2,List(4,6,8,10)))(functor.map(testNonEmpty)(_ * 2))

    val testFuncNonEmpty = NonEmpty[Int => Int](_ + 1, List(_ + 2, _ + 3))
    val testFuncNonEmpty2 = NonEmpty[Int => Int](_ * 1, List(_ * 2, _ * 3))
    val applicativeLaws = applicative.applicativeLaws
    assert(applicativeLaws.identity(testNonEmpty))
    assert(applicativeLaws.composition(testFuncNonEmpty, testFuncNonEmpty2, testNonEmpty))
    assert(applicativeLaws.homomorphism[Int, Int](_ * 5, 5))
    assert(applicativeLaws.interchange(testFuncNonEmpty, 5))
    val applTest = NonEmpty(1, List(2))
    val applFunctionTest = NonEmpty[Int => Int](_ + 1, List(_ * 2))
    assertResult(NonEmpty(2, List(3, 2, 4)))(applicative.ap(applTest)(applFunctionTest))

    val monadLaws = monad.monadLaws
    assert(monadLaws.leftIdentity[Int, Int](15, x => NonEmpty(x, List(x, x, x))))
    assert(monadLaws.rightIdentity(testNonEmpty))
    assert(monadLaws.associativity[Int, Int, Int](testNonEmpty, x => NonEmpty(x, List(x, x, x)), y => NonEmpty(y * 2, Nil)))
    val monadTest = NonEmpty(1, List(2,3))
    assertResult(NonEmpty(1, List(2,2,3,3,3)))(monad.flatMap(monadTest)(x => List.fill(x)(x) match {
      case head :: rest => NonEmpty(head, rest)
    }))
  }
//
//  test("Parser") {
//    import Parser._
//    val empty = ""
//    val digits = "123"
//    val letters = "abc"
//    assertResult(Some((), empty))(ok.runParser(empty))
//    assertResult(Some((), digits))(ok.runParser(digits))
//
//    assertResult(Some(((), "")))(eof.runParser(empty))
//    assertResult(None)(eof.runParser(digits))
//
//    assertResult(Some(('1', "23")))(satisfy(_.isDigit).runParser(digits))
//    assertResult(None)(satisfy(_.isDigit).runParser(letters))
//
//    assertResult(Some('a', "bc"))(element('a').runParser(letters))
//    assertResult(None)(element('1').runParser(letters))
//
//    assertResult(Some("ab", "c"))(stream("ab").runParser(letters))
//    assertResult(None)(stream("ab").runParser(digits))
//
//
//  }
//
//  test("Parser combinators") {
//    import Parser._
//    assertResult(Some("123", ""))(
//      monad.flatMap(satisfy(_.isDigit))(one =>
//        monad.flatMap(satisfy(_.isDigit))(two =>
//          monad.flatMap(satisfy(_.isDigit))(three =>
//            monad.flatMap(eof)(_ =>
//              monad.point(one.toString + two.toString + three.toString)
//            )
//          )
//        )
//      ).runParser("123")
//    )
//
//    val correctAb = "ababababababababababab"
//    val incorrectAB = "ababababbababababab"
//    assert(ab.runParser(correctAb).nonEmpty)
//    assert(ab.runParser(incorrectAB).isEmpty)
//
//    val integer = "12314"
//    assertResult(Some(12314, ""))(Parser.integer.runParser(integer))
//    val integer1 = "-1214"
//    assertResult(Some(-1214, ""))(Parser.integer.runParser(integer1))
//    val integer2 = "+123114"
//    assertResult(Some(123114, ""))(Parser.integer.runParser(integer2))
//    val incorrectInts = List("a123", "+-12", "--123", "123a", "12a33", "9999_1")
//    assert(incorrectInts.map(Parser.integer.runParser).map(_.isEmpty).forall(_ == true))
//  }
//
//  test("Parser bonus") {
//    import Parser._
//    val correctBrackets = "(((()))((())()))"
//    val incorrectBrackets = "(((()))((())()))("
//    val incorrectBrackets2 = "(((()))((()))()))"
//    assert(brackets.runParser(correctBrackets).nonEmpty)
//    assert(brackets.runParser(incorrectBrackets).isEmpty)
//    assert(brackets.runParser(incorrectBrackets2).isEmpty)
//  }
}
