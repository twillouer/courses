package objsets

import scala.annotation.tailrec

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + " " +
      "Text: " + text + " [" + retweets + "]"
}

/**
 * TweetSet is an immutable Set of Tweet.
 * A Tweet is unique by his text and cannot be present more than once.
 *
 * For efficient purpose, we classified the tweets with a Tree depending of the size of the text.
 *
 * Tweet equality is based on the tweet's text (see `def incl`). Hence, a `TweetSet`
 * could not contain two tweets with the same text from different users.
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * must be implemented in the subclass.
   */
  def filter(p: Tweet => Boolean): TweetSet

  /**
   * First call must be done with acc = new Empty
   *
   * The acc is the accumulator who will be returning when complete is ok.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Do an union between to TweetSet, as specified, all double tweet will be unified.
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the smallest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   */
  def mostRetweeted: Tweet
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   */
  def descendingByRetweet: TweetList = new Cons(this.mostRetweeted, this.remove(this.mostRetweeted).descendingByRetweet)

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which contains all elements of this set minus and the
   * the new element `tweet`..
   *
   * If `!this.contains(tweet)`, the current set is returned.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def mostRetweeted: Tweet = throw new NoSuchElementException

  override def descendingByRetweet: TweetList = Nil

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def filter(p: Tweet => Boolean): TweetSet = this

  def union(that: TweetSet): TweetSet = that

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def toString = ""
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = if (!p(elem)) left.union(right).filterAcc(p, acc) else left.union(right).filterAcc(p, acc.incl(elem))

  def filter(p: Tweet => Boolean): TweetSet = if (p(elem)) left.filter(p).union(right.filter(p)).incl(elem) else left.filter(p).union(right.filter(p))

  def union(that: TweetSet): TweetSet = {
    //    def unionAcc(that: TweetSet, acc: TweetSet): TweetSet = {
    //
    //      acc
    //    }
    //    unionAcc(that, new Empty)
    left.union(right.union(that.incl(elem)))
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  @tailrec
  final def mostRetweeted: Tweet = {
    this.filter(_.retweets > elem.retweets) match {
      case x: Empty    => elem
      case x: NonEmpty => x.mostRetweeted
    }
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def size: Int
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  @tailrec
  final def apply(indice: Int): Tweet = {
    if (isEmpty) {
      throw new NoSuchElementException
    }
    indice match {
      case 0 => head
      case x => tail(indice - 1)
    }
  }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  def size = 0

  override def toString = ""
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  def size = 1 + tail.size
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  private def isTrending(text: String, keyWord: List[String]): Boolean = {
    keyWord.find(text.contains(_)).isDefined
    //        keyWord match {
    //          case scala.collection.immutable.Nil  => false
    //          case x :: tail if (text.contains(x)) => true
    //          case x :: tail                       => isTrending(text, tail)
    //        }
  }

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(t => isTrending(t.text, google))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t => isTrending(t.text, apple))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
  println(GoogleVsApple.trending.size)
}
