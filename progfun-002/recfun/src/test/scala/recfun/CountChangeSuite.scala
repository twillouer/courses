package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange

  test("countChange: 50 self and other") {
    assert(countChange(50, List(50, 100, 200, 500)) === 1)
  }

  test("countChange: no coin") {
    assert(countChange(6, Nil) === 0)
  }

  test("countChange: 0") {
    assert(countChange(0, List(1)) === 0)
  }
  test("countChange: 6/2,3,4") {
    assert(countChange(6, List(2, 3, 4)) === 3)
  }
  test("countChange: 6/2,3,4,6") {
    assert(countChange(6, List(2, 3, 4, 6)) === 4)
  }
  test("countChange: 50 self") {
    assert(countChange(50, List(50)) === 1)
  }
  test("countChange: sorted CHF") {
    assert(countChange(300, List(5, 10, 20, 50, 100, 200, 500)) === 1022)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300, List(500, 5, 50, 100, 20, 200, 10)) === 1022)
  }

  test("countChange: example given in instructions") {
    assert(countChange(4, List(1, 2)) === 3)
  }

  test("countChange: example given in instructions reversed") {
    assert(countChange(4, List(2, 1)) === 3)
  }

  test("countChange: 3/1,2") {
    assert(countChange(3, List(1, 2)) === 2)
  }

  test("countChange: other") {
    assert(countChange(5, List(1, 2, 3)) === 5)
  }

  test("countChange: other with diff order") {
    assert(countChange(5, List(3, 2, 1)) === 5)
  }
  test("countChange: no pennies") {
    assert(countChange(301, List(5, 10, 20, 50, 100, 200, 500)) === 0)
  }
}
