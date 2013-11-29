package funsets

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import FunSets.Set
import FunSets.contains
import FunSets.diff
import FunSets.exists
import FunSets.filter
import FunSets.forall
import FunSets.intersect
import FunSets.map
import FunSets.multiSet
import FunSets.printSet
import FunSets.singletonSet
import FunSets.union
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val all = union(s3, union(s1, s2))
    //    val all = multiSet(1, 2, 3)
    //    val all = union(s1, s2, s3)

    val inf5: Set = x => x < 5
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elements") {
    new TestSets {
      assert(!contains(intersect(s1, s2), 1), "Intersect 1")
      assert(contains(intersect(s1, s1), 1), "Intersect 2")
      assert(contains(intersect(all, x => 1 == x), 1), "contains 1")
      assert(contains(intersect(all, x => 1 == x || x == 4), 1), "contains 1")
    }
  }

  test("diff contains all elements") {
    new TestSets {
      assert(!contains(diff(s1, s1), 1), "Diff 1")
      assert(!contains(diff(s1, s1), 2), "Diff 2")
      assert(!contains(diff(s1, s2), 2), "Diff 3")
      assert(!contains(diff(union(s1, s2), s2), 2), "Diff 4")
      assert(contains(diff(union(s1, s2), s1), 2), "Diff 5")
    }
  }

  test("filter contains all elements") {
    new TestSets {
      assert(contains(filter(s1, x => x == 1), 1), "Diff 1")
      assert(!contains(filter(s2, x => x == 1), 1), "Diff 1")
    }
  }

  test("forall contains all elements") {
    new TestSets {
      assert(forall(s1, x => x == 1), "forall")
      assert(forall(s2, x => x == 2), "forall")
      assert(forall(s3, x => x == 3), "forall")
      assert(!forall(all, x => x == 4), "forall")

      val neg: Set = x => x < 0
      assert(forall(neg, x => x < 0), "forall neg")
      assert(!forall(neg, x => x == 0), "forall neg")

      assert(forall(all, x => x >= 1 || x <= 4), "forall 1")

      assert(forall(multiSet(-1000, 0, +1000), x => -1000 == x || x == 0 || x == +1000), "forall multi")

      assert(forall(multiSet(1, 2, 3, 4), x => x < 5), "forall inf 5")

    }
  }

  //  test("forall must be strictly less than 1000") {
  //    ???
  //  }

  test("exist") {
    new TestSets {
      assert(exists(all, x => 1 == x), "exists 1")
      assert(!exists(all, x => 0 == x), "0 doesn't exists")
      assert(exists(all, x => 2 == x), "exists 2")
      assert(exists(all, x => x == 1 || x == 4), "exists 1 or 4")
    }
  }

  test("map") {
    new TestSets {
      val s = multiSet(-1000, 0, 1000)
      printSet(map(s, x => x + 1))

      assert(!contains(map(all, x => x + 1), 0), "map 1")
      assert(contains(map(all, x => x + 1), 2), "map 2")
      assert(contains(map(all, x => x + 1), 3), "map 3")
      assert(contains(map(all, x => x + 1), 4), "map 4")
      assert(!contains(map(all, x => x + 1), 1), "map 5")

      assert(!contains(map(multiSet(1, 3, 4, 5, 7, 1000), x => x - 1), 1000), "map 1000");

      assert(!contains(map(multiSet(1, 3, 4, 5, 7, -1000), x => x + 1), -1000), "map -1000");
    }
  }

  test("map & forall") {
    val s: Set = x => (x & 1) == 1
    assert(forall(map(s, x => x * 2), x => (x & 1) == 0), "map & forall")
  }

  test("exist & filter") {
    //[Observed Error] org.scalatest.exceptions.TestFailedException: The set of all even numbers and 3 should contain an odd element, namely 3.
  }
}
    