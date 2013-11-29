package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @TODO: write more tests.
 */

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val allTweets = TweetReader.allTweets
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("perf filter") {
    val r = TweetReader.allTweets.filter(_.retweets > 30)
    assert(size(r) === 293)
  }

  test("perf union") {
    val r = TweetReader.allTweets.union(TweetReader.allTweets)
    assert(size(r) === 695)
  }

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: retweets") {
    val t1 = new Tweet("1", "c body T", 7)
    val t2 = new Tweet("2", "d body T", 9)
    val t3 = new Tweet("3", "f body T", 10)
    val t4 = new Tweet("4", "e body T", 11)
    val set4 = new Empty().incl(t1).incl(t2).incl(t3).incl(t4)

    val filtered = set4.filter(_.retweets >= 10)

    assert(size(filtered) === 2)
    assert(filtered.contains(t3))
    assert(filtered.contains(t4))
  }

  test("filteracc: retweets") {
    val t1 = new Tweet("1", "c body T", 7)
    val t2 = new Tweet("2", "d body T", 9)
    val t3 = new Tweet("3", "f body T", 10)
    val t4 = new Tweet("4", "e body T", 11)
    val set4 = new Empty().incl(t1).incl(t2).incl(t3).incl(t4)

    val filtered = set4.filterAcc(_.retweets >= 10, new Empty())

    assert(size(filtered) === 2)
    assert(filtered.contains(t3))
    assert(filtered.contains(t4))
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union: with sorted set (2)") {
    val t1 = new Tweet("1", "c body T", 7)
    val t2 = new Tweet("2", "d body T", 9)
    val t3 = new Tweet("3", "f body T", 9)
    val t4 = new Tweet("4", "e body T", 9)
    val set4 = new Empty().incl(t1).incl(t4)
    val set3 = new Empty().incl(t2).incl(t3)

    val uni = set4.union(set3)

    assert(size(uni) === 4)
    assert(uni.contains(t1))
    assert(uni.contains(t2))
    assert(uni.contains(t3))
    assert(uni.contains(t4))
  }

  test("union: same is good(2)") {
    val t1 = new Tweet("1", "c body T", 7)
    val t2 = new Tweet("2", "d body T", 9)
    val t3 = new Tweet("3", "e body T", 10)
    val t4 = new Tweet("4", "f body T", 11)
    val set4 = new Empty().incl(t1).incl(t4).incl(t2).incl(t3)

    val uni = set4.union(set4)

    assert(size(uni) === 4)
    assert(uni.contains(t1))
    assert(uni.contains(t2))
    assert(uni.contains(t3))
    assert(uni.contains(t4))
  }

  test("most retweeted") {
    val t1 = new Tweet("1", "c body T", 7)
    val t2 = new Tweet("2", "d body T", 9)
    val t3 = new Tweet("3", "e body T", 10)
    val t4 = new Tweet("4", "f body T", 11)
    val set4 = new Empty().incl(t1).incl(t4).incl(t2).incl(t3)

    assert(set4.mostRetweeted === t4)
  }

  test("most retweeted on empty set") {
    intercept[NoSuchElementException] {
      new Empty().mostRetweeted
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending retweeted") {
    val t1 = new Tweet("1", "c body T", 7)
    val t2 = new Tweet("2", "d body T", 9)
    val t3 = new Tweet("3", "e body T", 10)
    val t4 = new Tweet("4", "f body T", 11)
    val set4 = new Empty().incl(t1).incl(t4).incl(t2).incl(t3)

    val result = set4.descendingByRetweet
    assert(result(0) === t4)
    assert(result(1) === t3)
    assert(result(2) === t2)
    assert(result(3) === t1)
  }

  test("descending retweeted with same retweeted") {
    val t1 = new Tweet("1", "c body T", 7)
    val t2 = new Tweet("2", "d body T", 9)
    val t3 = new Tweet("3", "e body T", 9)
    val t4 = new Tweet("4", "f body T", 11)
    val set4 = new Empty().incl(t1).incl(t4).incl(t2).incl(t3)

    val result = set4.descendingByRetweet
    assert(result(0) === t4)
    assert(result(1) === t3)
    assert(result(2) === t2)
    assert(result(3) === t1)
  }
}
