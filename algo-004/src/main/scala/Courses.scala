import com.sun.corba.se.impl.orbutil.graph.Graph
import java.io.{FileReader, BufferedReader, File}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import sun.awt.SunHints.Value

/**
 * User: william
 * Date: 14/07/13
 */
object Courses extends App {

  def readIntegerArray(fileName: String): List[Int] = {
    var lines = List[Int]()
    val input: BufferedReader = new BufferedReader(new FileReader(fileName))
    try {
      var line: String = null
      while (({
        line = input.readLine
        line
      }) != null) {
        lines = lines :+ (line.toInt)
      }
    }
    finally {
      input.close
    }
    lines
  }

  def readLongArray(fileName: String): Iterator[Long] = {
    Source.fromFile(fileName).getLines().map(_.toLong)
  }

  //    6
  //    5
  //    4
  //    3
  //    2
  //    1

  def countDivideAndConquer(list: Vector[Int]): (Vector[Int], Long) = {
    println(list)
    if (list.size <= 1) {
      (list, 0L)
    } else {
      val (leftV, rightV): (Vector[Int], Vector[Int]) = list.splitAt(list.size / 2)
      val (left, cleft) = countDivideAndConquer(leftV)
      val (right, cright) = countDivideAndConquer(rightV)
      var count: Long = cleft + cright
      var i_left = 0
      var i_right = 0
      var retour: Vector[Int] = Vector.empty
      var max = left.last
      val min = right.head
      while (retour.size < list.size) {
        if (i_left < left.size && (i_right == right.size || left(i_left) < right(i_right))) {
          retour = retour :+ left(i_left)
          if (left(i_left) > min) {
            count = count + i_right - 1
          }
          i_left = i_left + 1
        } else {
          retour = retour :+ right(i_right)
          if (right(i_right) < max) {
            count = count + left.size - i_left
          }
          i_right = i_right + 1
          if (i_right < right.size)
            max = right(i_right)
        }
      }
      (retour, count)
    }
  }

  def pivotFirst(list: ArrayBuffer[Int], startIndex: Int, endIndex: Int): Unit = {
  }

  def pivotLast(list: ArrayBuffer[Int], startIndex: Int, endIndex: Int): Unit = {
    swap(list, startIndex, endIndex - 1)
  }

  def pivotMiddle(list: ArrayBuffer[Int], startIndex: Int, endIndex: Int): Unit = {
    val middleIndex = (endIndex - 1 - startIndex) / 2 + startIndex
    val listL: List[(Int, Int)] = List((list(startIndex), startIndex), (list(endIndex - 1), endIndex - 1), (list(middleIndex), middleIndex))

    //        println(listL + "=> " + listL.sortBy(_._1).apply(1) + s" startIndex : $startIndex, endIndex : $endIndex")
    swap(list, startIndex, listL.sortBy(_._1).apply(1)._2)
  }

  def swap(list: ArrayBuffer[Int], first: Int, second: Int): Unit = {
    val valFirst = list(first);
    list.update(first, list(second))
    list.update(second, valFirst)
  }

  def quickSortCounting(list: ArrayBuffer[Int], pivotMethod: (ArrayBuffer[Int], Int, Int) => Unit): Long = {
    def quickSortCounting(startIndex: Int, endIndex: Int): Long = {
      if (endIndex - startIndex <= 1) {
        0L
      } else {
        try {
          pivotMethod(list, startIndex, endIndex)
          var i = startIndex + 1
          val pivot = list(startIndex)
          val nbSwap = endIndex - startIndex - 1
          (i until endIndex).foreach {
            j =>
            //                            println(s"i:$i/j:$j " + list(j) + " vs " + pivot)
              if (list(j) < pivot) {
                swap(list, i, j)
                i = i + 1
              }
            //                            nbSwap = nbSwap + 1
          }
          // Swap pivot at right place.
          swap(list, startIndex, i - 1)
          //                    nbSwap = nbSwap + 1
          //                    println(s"pivot : $pivot($startIndex/$endIndex) after : " + list.slice(startIndex, endIndex) + " list : " + list)

          val rLeft = quickSortCounting(startIndex, i - 1)
          val rRight = quickSortCounting(i, endIndex)

          nbSwap + rLeft + rRight
        } catch {
          case e: Throwable => {
            println(list);
            throw e
          }
        }
      }
    }
    quickSortCounting(0, list.size)
  }

  def firstProgrammingAssigment() {
//    val integers = readIntegerArray("IntegerArray.txt")

    //    val integers = List(1, 3, 5, 2, 4, 6).toVector // 3
    //        val integers = List(1,5,3,2,4).toVector     // 4
    //        val integers = List(5,4,3,2,1).toVector // 10

    //        val integers = List(9, 12, 3, 1, 6, 8, 2, 5, 14, 13, 11, 7, 10, 4, 0 ) // 56

    //        val integers = List( 37, 7, 2, 14, 35, 47, 10, 24, 44, 17, 34, 11, 16, 48, 1, 39, 6, 33, 43, 26, 40, 4, 28, 5, 38, 41, 42, 12, 13, 21, 29, 18, 3, 19, 0, 32, 46, 27, 31, 25, 15, 36, 20, 8, 9, 49, 22, 23, 30, 45) // 590
    //    countDivideAndConquer(integers.toVector)._1.foreach(println)
//    println("Result : " + countDivideAndConquer(integers.toVector)._2)
    println("Result : " + countDivideAndConquer(List(5,3,8,9,1,7,0,2,6,4).toVector)._2)
  }


  case class Graph(n: Int, others: List[Int]) {
    override def hashCode() = n

    override def equals(obj: Any): Boolean = obj match {
      case e: Graph => e.n == n
      case _        => false
    }
  }

  def readKarger(fileName: String): List[Graph] = {
    Source.fromFile(fileName).getLines().takeWhile(p => !p.isEmpty).map(p => {
      val tab = p.split(" |\t")
      Graph(tab(0).toInt, tab.drop(1).map(_.toInt).toSet.toList)
    }).toList
  }

  def readGraph(fileName: String): Map[Int, Graph] = {
    val graphs = new mutable.HashMap[Int, Graph]()
    Source.fromFile(fileName).getLines().takeWhile(p => !p.isEmpty).foreach(p => {
      val tab = p.split(" |\t")
      val n = tab(0).toInt
      val other = tab(1).toInt
      var g = graphs.get(n).getOrElse(Graph(n, List()))
      g = g.copy(others = g.others :+ other)
      graphs.update(n, g)

      if (!graphs.contains(other)) {
        graphs.update(other, Graph(other, List()))
      }

    })

    graphs.toMap
  }

  type Value = Int

  case class GraphCost(n: Int, others: List[(Int, Value)]) {
    override def hashCode() = n

    override def equals(obj: Any): Boolean = obj match {
      case e: GraphCost => e.n == n
      case _            => false
    }
  }

  def readCostGraph(fileName: String): Map[Int, GraphCost] = {
    val graphs = new mutable.HashMap[Int, GraphCost]()
    Source.fromFile(fileName).getLines().takeWhile(p => !p.isEmpty).foreach(p => {
      val tab = p.split(" |\t")
      val n = tab(0).toInt
      for (t <- tab.drop(1)) {
        if (t.trim.length > 1) {
          val (other, value) = t.trim.split(",") match {
            case Array(x, y) => (x.toInt, y.toInt)
          }
          var g = graphs.get(n).getOrElse(GraphCost(n, List()))
          g = g.copy(others = g.others :+(other, value))
          graphs.update(n, g)

          if (!graphs.contains(other)) {
            graphs.update(other, GraphCost(other, List()))
          }
        }
      }
    })

    graphs.toMap
  }

  def karger(graphs: Map[Int, Graph]): Map[Int, Graph] = {
    //    println(graphs)
    if (graphs.size <= 2) {
      //      println("end")
      return graphs
    }
    val graphNum1: Int = graphs.drop(Random.nextInt(graphs.size)).keysIterator.next()
    val s = Random.nextInt(graphs(graphNum1).others.size)
    val graphNum2: Int = graphs(graphNum1).others(s)
    //    println(s"\tcontract $graphNum1 et $graphNum2")

    // now contract == replace all occurence of "graphNum1" by "graphNum2"
    val newGraph = Graph(graphNum1, (graphs(graphNum1).others ::: graphs(graphNum2).others).map(p => if (p == graphNum2) graphNum1 else p).filterNot(p => p == graphNum1))

    karger((graphs.updated(graphNum1, newGraph).-(graphNum2)).map(g => (g._1, Graph(g._2.n, g._2.others.map(o => if (o == graphNum2) graphNum1 else o)))))
  }

  def secondProgrammingAssigment() {
    val integers = readIntegerArray("QuickSort.txt")

    // 10
    //        val integers = readIntegerArray("10.txt")
    //        1: 25
    //        2: 29
    //        3: 21
    // 100
    //                                        val integers = readIntegerArray("100.txt")
    //        1: 615
    //        2: 587
    //        3: 518
    // 100
    //                        val integers = readIntegerArray("1000.txt")
    //        1: 10297
    //        2: 10184
    //        3: 8921

    //        val integers = List(4, 5, 6, 7)
    //        val integers = List(8, 2, 4, 5, 7, 1)
    var buffer: ArrayBuffer[Int] = ArrayBuffer[Int]().++=(integers)
    //        println(buffer)
    println("1 : " + quickSortCounting(buffer, pivotFirst))
    //        println(buffer)
    buffer = ArrayBuffer[Int]().++=(integers)
    println("2 : " + quickSortCounting(buffer, pivotLast))
    buffer = ArrayBuffer[Int]().++=(integers)
    println("3 : " + quickSortCounting(buffer, pivotMiddle))
  }

  def thirdProgrammingAssigment() {
    val graphs = readKarger("kargerMinCut.txt")
    graphs.foreach(println)
    val mapGraphs = graphs.map(g => (g.n -> g)).toMap
    val result = karger(mapGraphs)
    println(result)

    val minResult = graphs.map(p => karger(mapGraphs).valuesIterator.next().others.size).sorted.iterator.next()

    println("Resultat : " + minResult)
  }

  def fourthProgrammingAssigment() {
    // Inverse arcs.
    def inverse(graphs: Array[Graph]): Map[Int, Graph] = {
      val inverted = new mutable.HashMap[Int, Graph]()
      graphs.foreach(g => {
        g.others.foreach(o => {
          val i = inverted.get(o).getOrElse(Graph(o, List()))
          inverted.update(o, i.copy(others = i.others :+ g.n))
          if (!inverted.contains(g.n)) {
            inverted.update(g.n, Graph(g.n, List()))
          }
        })
      })
      println("size : " + graphs.size + " vs " + inverted.size)
      inverted.toMap
    }

    def dfs(graphs: Array[Graph], g: Graph, visited: mutable.Buffer[Boolean], result: mutable.Buffer[Int], count: Int): Int = {
      var actualCount = count

      def dfs(g: Graph): Unit = {
        if (!visited(g.n)) {
          visited.update(g.n, true)
          g.others.foreach(o => dfs(graphs(o - 1)))
          result.update(g.n, actualCount)
          actualCount = actualCount - 1
        }
      }

      dfs(g)

      actualCount
    }

    type Value = Int

    def parcours1(graph: Array[Graph]): Map[Int, Value] = {
      val visited = (0 to graph.size).map(p => false).toBuffer
      //      (0 to graph.size).foreach(p => visited.update(p, false))
      val result = (0 to graph.size).map(p => -1).toBuffer
      var lastCount = graph.size
      graph.toList.sortBy(_.n).reverse.foreach(g => {
        if (!visited(g.n)) {
          lastCount = dfs(graph, g, visited, result, lastCount)
        }
      })

      var i: Int = 0
      result.map(v => {
        i = i + 1
        (i - 1, v)
      }).toMap
    }

    def parcours2(graph: Array[Graph], sortedGraph: Map[Int, Value]): Array[Int] = {
      val visited = (0 to graph.size).map(p => false).toBuffer

      val result = (0 to graph.size).map(p => -1).toBuffer
      var output = Array[Int](0, 0, 0, 0, 0)
      var lastCount = graph.size
      sortedGraph.toList.sortBy(_._2).map(_._1).filterNot(n => n <= 0).foreach(n => {
        val g = graph(n - 1)
        if (!visited(g.n)) {
          val saveCount = lastCount
          lastCount = dfs(graph, g, visited, result, lastCount)
          //          output = output :+ ()
          val counted = saveCount - lastCount
          if (output.head < counted) {
            output(0) = counted
            output = output.sorted
          }
        }
      })
      output.sorted.reverse
    }

    val s = System.currentTimeMillis()
    val graphs = readGraph("SCC.txt")
    //        val graphs = readGraph("SCCtestcase1.txt") // 3,3,3,0,0
    //            val graphs = readGraph("SCCtestcase2.txt") // 3,3,2,0,0
    //        val graphs = readGraph("SCCtestcase3.txt") // 3,2,2,2,1

    println("read done in " + (System.currentTimeMillis() - s) + ", now inverse")

    val orderedGraph: Array[Courses.Graph] = graphs.toArray.sortBy(_._1).map(_._2) //.toArray
    val inversed = inverse(orderedGraph)
    println("inverse done, now parcours1")
    val intermediate = parcours1(inversed.toArray.sortBy(_._1).map(_._2).toArray)
    println("parcours1 done, now parcours2")
    val result = parcours2(orderedGraph, intermediate)
    println(result.toVector)
  }

  def fifthProgrammingAssigment() {

    /**
     * Parcours le graphe de start_v à end_v.
     * @param graphs
     * @param start_v
     * @param end_v
     */
    def parcours(graphs: Map[Int, GraphCost], start_v: Int, end_v: Int): Value = {
      // initialize
      val initDist: ArrayBuffer[Value] = ArrayBuffer() ++ (0 to graphs.size).map(p => 1000000).toArray
      val visited: ArrayBuffer[Boolean] = ArrayBuffer() ++ (0 to graphs.size).map(p => false).toArray

      def findMin(): (Int, Value) = {
        var min: Value = 1000000
        var indice = 0
        var i = -1
        for (v <- initDist) {
          i = i + 1
          if (!visited(i)) {
            if (v < min) {
              min = v
              indice = i
            }
          }
        }
        (indice, min)
      }


      def initialized(g: GraphCost, d: Value): Unit = {
        visited.update(g.n, true)
        g.others.foreach(p => initDist.update(p._1, Math.min(p._2 + d, initDist(p._1))))
      }

      visited.update(0, true)
      var allDone = false
      initDist(start_v) = 0
      while (!allDone) {
        val (next, value) = findMin()
        //        println(s"next:$next, value:$value (" + initDist.drop(0).mkString(","))
        if (next == end_v || next == 0) {
          allDone = true
        } else {
          val g = graphs(next)
          initialized(g, value)
        }
      }
      //      println(initDist.drop(1).mkString(","))
      initDist(end_v)
    }

    // dijkstraData.txt
    //    val result = parcours(readCostGraph("dijkstraTest1.txt"), 1, 4) // Result : 2
    //    val result = parcours(readCostGraph("dijkstraTest2.txt"), 1, 7) // Result : 5
    //        val result = parcours(readCostGraph("dijkstraTest3.txt"), 13, 5) // Result : 26
    //        val result = parcours(readCostGraph("dijkstraTest4.txt"), 28, 6) // Result : 9, path : 28, 16, 6
    //    val result = parcours(readCostGraph("dijkstraTest5.txt"), 1, 10) // Result : 487

    val graphs = readCostGraph("dijkstraData.txt")
    val to = List(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
    println(to.map(parcours(graphs, 1, _)).mkString(",")) // 2599,2610,2947,2052,2367,2399,2029,2442,2505,3068

  }


  def sixthProgrammingAssigment() = {

    def twoSumProblem(ints: Iterator[Long]): Long = {
      //      val map = ints.map(i => (i.hashCode(), i)).toMap
      var set = new mutable.TreeSet[Long]()
      set ++= ints
      val l = ints.toList
//      var set = MyTree(l, 1000)
      println("Mapping ok : ")

      def canDo(t: Long): Option[(Long, Long)] = {
        t.hashCode()
        if (t % 1000 == 0) println(s"canDo $t ${System.currentTimeMillis()}")
        l.foreach(p => {
          val u = t - p
          if (u != p && set.contains(u)) {
            return Some((p, u))
          }
        })
        None
      }

      (-10000 to 10000).count(canDo(_).isDefined)
    }


    println("Reading...")
    val ints = readLongArray("2sum.txt")
//    val ints = readLongArray("2sumTest1.txt")
    println("Reading ok.")
    val result = twoSumProblem(ints)
    println(result)
  }

  def sixthMedian() = {
    var hLow = mutable.LinkedList[Long]()
    var hhigh = mutable.LinkedList[Long]()

    def median(n: Long): Long = {
      if (hhigh.headOption.isEmpty || hhigh.head < n) {
        //      if (hLow.lastOption.isEmpty || hLow.last < n) {
        hhigh = (hhigh :+ n).sorted
        //        hhigh.head
      } else {
        hLow = (hLow :+ n).sorted
        //        hLow.last
      }
      val sum = hhigh.size + hLow.size
      if (hhigh.size < (sum / 2)) {
        hhigh = (hhigh :+ hLow.last).sorted
        hLow = hLow.take(hLow.size - 1)
      } else if (hLow.size < sum / 2) {
        hLow = (hLow :+ hhigh.head).sorted
        hhigh = hhigh.tail
      }

      //      println("\t low :" + hLow + " high : " + hhigh)

      //      val result = if(hLow.lastOption.isEmpty || sum % 2 == 1 && hhigh.headOption.isDefined) {
      val result = if (hLow.size < hhigh.size) {
        hhigh.head
      } else {
        hLow.last
      }

      //      println("median : " + result)
      result
    }

    //    val ints = readLongArray("medianTest1.txt") // Result = 54
    //    val ints = readLongArray("medianTest2.txt") // Result = 23 All medians: [3, 3, 4, 3, 3, 3, 4]
    val ints = readLongArray("median.txt") // Result = 54
    val result = ints.map(median(_)).sum
    println("Result : " + result + " =>" + (result % 10000))
  }

      sixthProgrammingAssigment()
//  sixthMedian()
//  firstProgrammingAssigment();
}
