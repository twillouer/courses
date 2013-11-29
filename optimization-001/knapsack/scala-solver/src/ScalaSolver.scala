/**
 * User: william
 * Date: 24/06/13
 */

import java.io.{IOException, FileReader, BufferedReader}


object ScalaSolver extends App {
    def println(o: => Object) {
        if (false) {
            Console.err.println(o)
        }
    }

    case class Knap(value: Int, weight: Int, pos: Int) {
        lazy val ratio: Double = value.toDouble / weight.toDouble
    }

    object ListKnap {
        val empty = ListKnap(List(), 0, 0)

        def apply(v: List[Knap]): ListKnap = ListKnap(v, sum(v), weight(v))

        private def sum(v: List[Knap]) = v.foldLeft(0)((s, k) => s + k.value)

        private def weight(v: List[Knap]) = v.foldLeft(0)((s, k) => s + k.weight)
    }

    case class ListKnap(v: List[Knap], sum: Int, weight: Int) {
        lazy val isEmpty = v.isEmpty
        lazy val head = v.head
        lazy val tail = ListKnap(v.tail, sum - v.head.value, weight - v.head.weight)

        def :+(e: Knap) = ListKnap(v :+ e, sum + e.value, weight + e.weight)
    }

    def time[T](f: => T): T = {
        val start = System.nanoTime()
        try {
            return f
        } finally {
            if (false) Console.err.println(System.nanoTime() - start + " ns")
        }
    }

    /**
     * Permet d'utiliser le "dynamic programming".
     * @return
     */
    def dynamicSolve(weight: List[Int], values: List[Int], capacity: Int): List[Knap] = {
        var i = 0
        val initKnaps = values.zip(weight).map(t => {
            i += 1
            Knap(t._1, t._2, i)
        }).toList

        type Weight = Int
        type Value = Int

        var solver = List[Vector[(Value, Weight)]]()
        solver :+= (0 to capacity).par.map(p => (0, 0)).toVector
        Console.err.println("Done " + (100D * solver.size.toDouble / capacity.toDouble) + "%")
        for (k <- initKnaps) {
            val previous: Vector[(Value, Weight)] = solver.last
            val actual: Vector[(Value, Weight)] = (0 until previous.size).map(i => {
                val (pV, pW) = previous(i)
                if (i < k.weight || i < k.weight) {
                    (pV, pW)
                } else {
                    val addWeight = pW + k.weight
                    if (addWeight <= i) {
                        (pV + k.value, addWeight)
                    } else {
                        val pDec = previous(i - k.weight)
                        val testedValue = pDec._1 + k.value
                        val testedWeight = pDec._2 + k.weight
                        if (testedWeight <= i && testedValue > pV) {
                            (testedValue, testedWeight)
                        } else {
                            (pV, pW)
                        }
                    }
                }
            }).toVector
            solver :+= actual
            Console.err.println("Done " + (100D * solver.size.toDouble / capacity.toDouble) + "%")
        }
        //        solver.foreach(println(_))

        Console.println(solver.last.last)

        // TODO : retrouver la solution depuis.

        Nil
    }

    def estimate(actual: ListKnap, knaps: ListKnap, capacity: Int): Int = {
        var estim = actual.sum
        var weight = actual.weight
        val iterator = knaps.v.iterator
        while (weight != capacity && iterator.hasNext) {
            val k = iterator.next()
            val tweight = weight + k.weight
            if (tweight <= capacity) {
                estim += k.value
                weight = tweight
            } else if (weight != capacity) {
                estim += (k.value / (k.weight.toDouble / (capacity - weight).toDouble)).toInt
                weight = capacity
            }
        }

        estim
    }

    /**
     * Create and sort Knap by ratio.
     *
     * @param values
     * @param weight
     * @return
     */
    def sortKnaps(values: List[Int], weight: List[Int]): List[ScalaSolver.Knap] = {
        var i = 0
        values.zip(weight).map(t => {
            i += 1
            Knap(t._1, t._2, i)
        }).sortBy(_.ratio).reverse.toList
    }

    /**
     * On va dans l'arbre en descendant jusqu'aux feuilles et en remontant.
     */
    def depthFirstBoundingBranch(weight: List[Int], values: List[Int], capacity: Int): ListKnap = {
        val initKnaps: List[ScalaSolver.Knap] = sortKnaps(values, weight)

        println("knaps : " + initKnaps)
        var solEstimate = 0
        var solution = ListKnap.empty

        def depthFirstBoundingBranch(knaps: ListKnap, actual: ListKnap, actualCap: Int): Unit = {
            if (actualCap < 0) {
                return
            }
            if (actual.weight > capacity) {
                return
            }
            val estim = estimate(actual, knaps, capacity)
            if (knaps.isEmpty) {
                if (actual.sum > solution.sum) {
                    solution = actual
                    solEstimate = estim
                }
                return
            }

            if (estim < solEstimate) {
                return
            }

            val k = knaps.head
            val tail = knaps.tail

            depthFirstBoundingBranch(tail, actual, actualCap)
            depthFirstBoundingBranch(tail, actual :+ k, actualCap - k.weight)
        }

        depthFirstBoundingBranch(ListKnap(initKnaps), ListKnap.empty, capacity)
        solution
    }

    /**
     * Try solution of best first.
     *
     * @param weight
     * @param values
     * @param capacity
     * @return
     */
    def bestFirst(weight: List[Int], values: List[Int], capacity: Int): ListKnap = {
        val initKnaps: List[ScalaSolver.Knap] = sortKnaps(values, weight)

        println("initKnaps : " + initKnaps)
        var solEstimate = 0
        var solution = ListKnap.empty

        var i = 0

        def bestFirst(knaps: ListKnap, actual: ListKnap, actualCap: Int): Unit = {

            i += 1
            val id = i
            println(s"[$id]" + actual + " " + actual.sum + " max : " + solution.sum)

            if (actualCap < 0) {
                return
            }
            if (actual.weight > capacity) {
                return
            }
            val estim = estimate(actual, knaps, capacity)
            if (knaps.isEmpty) {
                if (actual.sum > solution.sum) {
                    solution = actual
                    solEstimate = estim
                    println(s"[$id]" + s"found a solution : $solution")
                }
                return
            }

            if (estim < solEstimate) {
                return
            }

            val k = knaps.head
            val tail = knaps.tail

            val actualWith = actual :+ k

            if (actualWith.weight <= capacity) {
                val estimateWith = estimate(actualWith, tail, capacity)
                val estimateWithout = estimate(actual, tail, capacity)
                if (estimateWith > estimateWithout) {
                    bestFirst(tail, actualWith, actualCap - k.weight)
                    bestFirst(tail, actual, actualCap)
                } else {
                    bestFirst(tail, actual, actualCap)
                    bestFirst(tail, actualWith, actualCap - k.weight)
                }
            } else {
                bestFirst(tail, actual, actualCap)
            }
        }

        bestFirst(ListKnap(initKnaps), ListKnap.empty, capacity)
        solution
    }

    /**
     * Read the instance, depthFirstBoundingBranch it, and print the solution in the standard output
     */
    def solve(args: Seq[String]) {
        var fileName: String = null
        for (arg <- args) {
            if (arg.startsWith("-file=")) {
                fileName = arg.substring(6)
            }
        }
        if (fileName == null) return
        var lines = List[String]()
        val input: BufferedReader = new BufferedReader(new FileReader(fileName))
        try {
            var line: String = null
            while ((({
                line = input.readLine;
                line
            })) != null) {
                lines = lines :+ (line)
            }
        }
        finally {
            input.close
        }
        val firstLine: Array[String] = lines(0).split("\\s+")
        val items: Int = Integer.parseInt(firstLine(0))
        val capacity: Int = Integer.parseInt(firstLine(1))
        var values: List[Int] = Nil
        var weights: List[Int] = Nil

        var i = 0
        lines.tail.foreach(line => {
            i += 1
            if (i <= items) {
                val parts: Array[String] = line.split("\\s+")

                values = values :+ (java.lang.Integer.parseInt(parts(0)))
                weights = weights :+ java.lang.Integer.parseInt(parts(1))
            }
        })

        //        time(dynamicSolve(weights, values, capacity))
//        var result = time(depthFirstBoundingBranch(weights, values, capacity))
//
//        System.out.println(result.sum + " 0")
//        (Range(1, (items + 1))).foreach(a => Console.print(if (result.v.exists(p => p.pos == a)) "1 " else "0 "))
//        println("")

        var result = time(bestFirst(weights, values, capacity))
        System.out.println(result.sum + " 0")
        (Range(1, (items + 1))).foreach(a => Console.print(if (result.v.exists(p => p.pos == a)) "1 " else "0 "))
        println("")


        var value: Int = 0
        var weight: Int = 0
        val taken: Array[Int] = new Array[Int](items)

        {
            var i: Int = 0
            while (i < items) {
                {
                    if (weight + weights(i) <= capacity) {
                        taken(i) = 1
                        value += values(i)
                        weight += weights(i)
                    }
                    else {
                        taken(i) = 0
                    }
                }
                ({
                    i += 1;
                    i - 1
                })
            }
        }
        //        System.out.println(value + " 0")
        //        List.fromArray(taken).foreach(t => Console.print(t + " "))
        //        System.out.println("")
    }

    /**
     * The main class
     */
    try {
        solve(args.toList)
    }
    catch {
        case e: IOException => {
            e.printStackTrace
        }
    }
}
