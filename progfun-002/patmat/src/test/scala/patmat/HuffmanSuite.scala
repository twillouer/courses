package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a big tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(List(('a', 2), ('b', 1)) === times(List('a', 'b', 'a')))
  }

  test("singleton") {
    assert(singleton(List(Leaf('a', 2))))
  }

  test("singleton !") {
    assert(!singleton(List(Leaf('a', 2), Leaf('a', 3))))
  }

  test("singleton fork") {
    assert(singleton(List(Fork(Leaf('a', 2), Leaf('b', 3), "ab".toList, 1))))
  }

  test("singleton !fork") {
    assert(!singleton(List(Fork(Leaf('a', 2), Leaf('b', 3), "ab".toList, 1), Fork(Leaf('a', 2), Leaf('b', 3), "ab".toList, 1))))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("#combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of some leaf list with weight") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 4), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 1), Leaf('t', 4), List('e', 't'), 5)))
  }

  test("until first") {
    val t = makeOrderedLeafList(times(string2Chars("AAAAAAAABBBCDEFGH")))

    assert(t === List(Leaf('E', 1), Leaf('F', 1), Leaf('G', 1), Leaf('C', 1), Leaf('H', 1), Leaf('D', 1), Leaf('B', 3), Leaf('A', 8)))
    val r = until(singleton, combine)(t)

    assert(r === List(Fork(Leaf('A', 8), Fork(Fork(Fork(Leaf('E', 1), Leaf('F', 1), List('E', 'F'), 2), Fork(Leaf('G', 1), Leaf('C', 1), List('G', 'C'), 2), List('E', 'F', 'G', 'C'), 4), Fork(Fork(Leaf('H', 1), Leaf('D', 1), List('H', 'D'), 2), Leaf('B', 3), List('H', 'D', 'B'), 5), List('E', 'F', 'G', 'C', 'H', 'D', 'B'), 9), List('A', 'E', 'F', 'G', 'B', 'C', 'H', 'D'), 17)))
  }

  test("decode D") {
    val r = Fork(Fork(Fork(Fork(Fork(Fork(Fork(Leaf('E', 1), Leaf('F', 1), List('E', 'F'), 2), Leaf('G', 1), List('E', 'F', 'G'), 3), Leaf('C', 1), List('E', 'F', 'G', 'C'), 4), Leaf('H', 1), List('E', 'F', 'G', 'C', 'H'), 5), Leaf('D', 1), List('E', 'F', 'G', 'C', 'H', 'D'), 6), Leaf('B', 3), List('E', 'F', 'G', 'C', 'H', 'D', 'B'), 9), Leaf('A', 8), List('E', 'F', 'G', 'B', 'C', 'H', 'D', 'A'), 17)
    assert(decode(r, List(1)) === List('A'))
    assert(decode(r, List(0, 0, 1)) === List('D'))
  }

  test("createCodeTree") {
    val text = "some very long text"
    val r = createCodeTree(text.toList)

    assert(weight(r) == text.size)
  }

  test("decode secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert") {
    val r = Fork(Fork(Fork(Fork(Fork(Fork(Fork(Leaf('E', 1), Leaf('F', 1), List('E', 'F'), 2), Leaf('G', 1), List('E', 'F', 'G'), 3), Leaf('C', 1), List('E', 'F', 'G', 'C'), 4), Leaf('H', 1), List('E', 'F', 'G', 'C', 'H'), 5), Leaf('D', 1), List('E', 'F', 'G', 'C', 'H', 'D'), 6), Leaf('B', 3), List('E', 'F', 'G', 'C', 'H', 'D', 'B'), 9), Leaf('A', 8), List('E', 'F', 'G', 'B', 'C', 'H', 'D', 'A'), 17)
    val c = convert(r)
    assert(c.find(p => p._1 == 'A').get._2 === List(1))
    assert(c.find(p => p._1 == 'D').get._2 === List(0, 0, 1))
  }

  test("quickencode") {
    assert(secret == quickEncode(frenchCode)("huffmanestcool".toList))
  }

  test("quickencode empty") {
    assert(List() == quickEncode(frenchCode)("".toList))
  }

  test("merge code table") {
    val a = List(('a', List(0, 1)), ('b', List(1, 0)))
    mergeCodeTables(a, a)
  }
}
