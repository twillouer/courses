/**
 * User: william
 * Date: 24/06/13
 */

import java.io.{IOException, FileReader, BufferedReader}
import java.util.Date
import scala.collection.immutable.{TreeMap, SortedMap}
import scala.util.Try


object Functions {
    def println(o: => Any) {
        if (false) {
            Console.err.println(o)
        }
    }

    def pr[T](o: => T): T = {
        val r = o
        println(r)
        r
    }

    def pr[T](msg: => String, o: => T): T = {
        val r = o
        println(s"$msg = $r")
        r
    }

    def logResult[T](o: => T): T = {
        val r = o
        println(r)
        r
    }

    def logResult[T](str: String, o: => T): T = {
        val r = o
        println(str + " " + r)
        r
    }

    def time[T](f: => T): T = {
        val start = System.currentTimeMillis()
        try {
            return f
        } finally {
            val now = System.currentTimeMillis()
            println(new Date(now) + "" + (now - start) + " ms")
        }
    }
}


object EdgeSolver extends App {

    import Functions._

    case class Edge(n1: Int, n2: Int)

    case class GraphEdge(n: Int, relations: List[Int]) {
        override def toString() = s"GraphEdge($n)"
    }

    case class GraphEdges(edges: List[GraphEdge]) {

        lazy val mapEdges = edges.map(e => (e.n, e)).toMap

        /**
         *
         * @param alreadyVisited
         * @return
         */
        def graphDegrees(alreadyVisited: Set[GraphEdge]): List[(GraphEdge, Int)] = {
            var mapDegree = Map[Int, Int]()
            edges.foreach(e => {
                mapDegree = mapDegree.updated(e.n, mapDegree.getOrElse(e.n, 0) + e.relations.size)
                e.relations.foreach(r => mapDegree = mapDegree.updated(r, mapDegree.getOrElse(r, 0) + 1))
            })
            alreadyVisited.foreach(e => mapDegree.updated(e.n, mapDegree.getOrElse(e.n, 1) - e.relations.size))
            edges.map(e => (e, mapDegree.getOrElse(e.n, 0))).sortBy(_._2).reverse
        }

        def apply(n: Int): GraphEdge = mapEdges(n)
    }

    case class Edges(edges: List[Edge]) {
        lazy val graphEdges = {
            val ge = edges.groupBy(_.n1).map(me => GraphEdge(me._1, me._2.map(e => e.n2))).toList
            val startingEdges = edges.map(_.n1).toSeq
            val missingEdges = edges.map(_.n2).filterNot(startingEdges.contains(_)).toSet.toList
            GraphEdges(ge ::: missingEdges.map(p => GraphEdge(p, Nil)))
        }

        /**
         *
         * @param alreadyVisited graph.
         * @return
         */
        def degrees(alreadyVisited: Seq[Int]): List[(Edge, Int)] = {
            var mapDegree = Map[Int, Int]()
            edges.foreach(e => {
                mapDegree = mapDegree.updated(e.n1, mapDegree.getOrElse(e.n1, 0) + 1)
                mapDegree = mapDegree.updated(e.n2, mapDegree.getOrElse(e.n2, 0) + 1)
            })
            alreadyVisited.foreach(e => mapDegree.updated(e, mapDegree.getOrElse(e, 1) - 1))
            edges.map(e => (e, mapDegree.getOrElse(e.n1, 0)))
        }
    }

    type Color = Int


    /**
     * One color for each edge.
     *
     * @param edges
     * @param nodeCount
     * @return
     */
    def naiveConstraintProgramming(edges: List[Edge], nodeCount: Int): List[Int] = {
        (0 until nodeCount).map(i => i).toList
    }

    /**
     * Matrice of boolean.
     *
     * @param edges
     * @param nodeCount
     * @return
     */
    def simpleConstraintProgramming(edges: List[Edge], nodeCount: Int): Seq[Int] = {
        // X = Color, Y = Edge
        val init = (0 until nodeCount).map(i => true).toVector
        var domain = (0 until nodeCount).map(j => init).toVector

        def apply(edge: Edge): Unit = {
            val color1 = color(edge.n1)
            val color2 = color(edge.n2)
            if (color1 >= 0 && color1 == color2) {
                domain = domain.updated(edge.n2, domain(edge.n2).updated(color1, false))
            }
        }

        def color(iedge: Int): Color = {
            domain(iedge).indexOf(true)
        }

        def colors() = {
            domain.map(p => p.indexOf(true))
        }

        do {
            (0 until edges.size).foreach(i => {
                apply(edges(edges.size - 1 - i))
                // Apply
                edges.slice(0, i).foreach(apply)
            })
        } while (!testingAgainsConstrains(colors, edges).isEmpty)

        //        println("Count : " + domain.map(p => p.count(v => !v)).sum)
        colors
    }

    /**
     * Increment of each colors until it's ok.
     *
     * @param edges
     * @param nodeCount
     * @return
     */
    def simpleConstraintProgramming2(edges: List[Edge], nodeCount: Int): Seq[Color] = {
        // X = Color, Y = Edge
        var domain: Vector[Color] = (0 until nodeCount).map(i => 0).toVector

        def apply(edge: Edge): Unit = {
            val color1 = color(edge.n1)
            val color2 = color(edge.n2)
            if (color1 >= 0 && color1 == color2) {
                domain = domain.updated(edge.n2, domain(edge.n2) + 1)
            }
        }

        def color(iedge: Int): Color = {
            domain(iedge)
        }

        def colors: Vector[Color] = {
            domain.map(p => p)
        }

        do {
            (0 until edges.size).foreach(i => {
                apply(edges(edges.size - 1 - i))
                // Apply
                edges.slice(0, i).foreach(apply)
            })
        } while (!testingAgainsConstrains(colors, edges).isEmpty)

        colors
    }

    /**
     * With a graph of edges.
     *
     * @param edges
     * @param nodeCount
     * @param maxColors
     * @return
     */
    def simpleConstraintProgramming4(edges: Edges, nodeCount: Int, maxColors: Int): Seq[Color] = {
        println(s"testing agains $maxColors colors")


        var dejaVu: Set[Domain] = Set.empty

        object Domain {
            var cacheRemove = Map[(List[Color], Color), List[Color]]()

            def remove(colors: List[Color], color: Color) = cacheRemove.getOrElse((colors, color), {
                val nVal = colors.filterNot(_ == color)
                cacheRemove += (colors, color) -> nVal
                nVal
            })

            def apply(nodeCount: Int, maxColors: Int, edges: GraphEdges): Domain = {
                val initColors = (0 until (maxColors)).toList
                new Domain(edges.edges.map(e => e -> initColors).toMap, edges, Set.empty)
                //                new Domain((0 until nodeCount).map(p => ((Math.min(nodeCount - p - 1, maxColors - 1)) to(0, -1)).toList).toList, edges)
            }
        }

        case class Domain(domain: Map[GraphEdge, List[Color]], edges: GraphEdges, alreadyVisited: Set[GraphEdge]) {
            //            override def toString = domain.map(_._1) mkString (" ")


            lazy val colors: List[Color] = {
                if (domain.values.exists(_.isEmpty))
                    Nil
                else
                    domain.toList.sortBy(_._1.n).map(p => p._2.head)
            }

            def apply(index: GraphEdge) = domain(index)

            def updated(index: GraphEdge, nRows: List[Color]) = Domain(domain.updated(index, nRows), edges, alreadyVisited)

            def remove(index: GraphEdge, color: Color) = Domain(domain.updated(index, Domain.remove(domain(index), color)), edges, alreadyVisited)

            lazy val hasSolution: Boolean = {
                !colors.isEmpty && !edges.edges.exists(e => {
                    val colors1 = domain(e)
                    e.relations.exists(n2 => colors1.union(domain(edges(n2))) == colors1)
                })
            }

            /**
             * Test constraints and return edges who are in errors.
             *
             * @return
             */
            lazy val areConstrainsOk: Boolean = {
                if (colors.isEmpty) false
                else !edges.edges.toStream.exists(e => e.relations.exists(n2 => colors(e.n) == colors(n2)))
            }

            /**
             * If an edge has a color, then prune the others
             * @return
             */
            private lazy val simplePruneDomain: Domain = {
                var nDomain = this
                domain.filter(_._2.size == 1).foreach(e => {
                    nDomain = e._1.relations.foldLeft(nDomain)((dom, n2) => dom.remove(edges(n2), e._2.head))
                })
                nDomain
            }

            /**
             * Tous ceux qui n'ont plus de relation avec d'autres voient leur couleur "fixée".
             */
            lazy val pruneDomain: Domain = {
                val uniqColors = graphDegrees.filter(_._2 == 0)
                uniqColors.foldLeft(simplePruneDomain)((nDomain, uColor) => nDomain.updated(uColor._1, List(nDomain.apply(uColor._1).head)))
            }

            lazy val maxColor: Color = domain.map(_._2.max).max

            lazy val graphDegrees: List[(GraphEdge, Int)] = edges.graphDegrees(alreadyVisited)

            /**
             * On cherche l'index suivant, qui a le plus de relations avec les autres.
             *
             * @return
             */
            lazy val nextEdge: Option[GraphEdge] = pr {
                val sorted: Seq[(GraphEdge, Int)] = graphDegrees.sortBy(_._2).reverse
                sorted /*.filterNot(_._2 == 0)*/ .filterNot(p => domain(p._1).isEmpty).map(_._1).find(e => !alreadyVisited.contains(e)).headOption
            }

            lazy val solutionsToTest: BigInt = domain.filterNot(_._2.size == 0).foldLeft(BigInt(1))((scalaire, e) => scalaire * e._2.size)

            def visitNow(edge: GraphEdge): Domain = this.copy(alreadyVisited = alreadyVisited + edge)

        }

        // X = Color, Y = Edge
        val initDomain: Domain = Domain(nodeCount, maxColors, edges.graphEdges)


        def colorsSolution(domain: Domain): Seq[Color] = {
            // Set solution with an impossible val  ue.
            var solution = Domain(nodeCount, nodeCount + 1, edges.graphEdges)
            var stop: Boolean = false
            //            var minimalSolution = solution.size + 1 // numberColors(solution)

            def deepThrow(domain: Domain, i: Option[GraphEdge]): Unit = {
                if (stop) {
                    println("stop")
                    return
                }
                println(s"domain : $domain/$i")

                //                println(dom)
                if (i.isEmpty) {
                    println("no index")
                    return
                }
                val index = i.get
                //                println(s"domain : $domain, index : $index")
                if (domain(index).size == 0) {
                    println("list of color empty")
                    return
                }
                if (!domain.hasSolution) {
                    println("domain doesn't have solution")
                    return
                }
                if (domain.maxColor >= solution.maxColor) {
                    println("too many colors " + domain.maxColor + " solution : " + solution.maxColor)
                    return
                }

                if (dejaVu.contains(domain)) {
                    //                    Console.err.println(s"On a déjà vu $domain")
                    return
                }
                dejaVu = dejaVu + domain
                if (dejaVu.size % 20000 == 0) {
                    Console.err.println(dejaVu.size + "" + new Date() + " " + i + " " + domain + " encore " + domain.solutionsToTest)
                }

                domain.colors match {
                    case Nil => return
                    case p   => {
                        if (domain.areConstrainsOk) {
                            //                            val sizeOfSol = numberColors(p)
                            //                            println("Constraints ok " + domain + " " + sizeOfSol + " vs " + minimalSolution)
                            if (domain.maxColor < solution.maxColor) {
                                println("Constraints ok " + p)
                                solution = domain
                                stop = true
                                return
                            }
                        }
                    }
                }
                val domain3 = domain.visitNow(index).pruneDomain
                (domain3(index)).foreach {
                    thisColor =>
                    //                    println(thisColor + " for index "+ index + " colors : " + domain3(index))
                        if (!stop) deepThrow(domain3.updated(index, List(thisColor)).pruneDomain, domain3.nextEdge)
                }
                if (!stop) deepThrow(domain3, domain3.nextEdge)

                //                domain2(index) match {
                //                    case Nil       =>
                //                    case e :: tail => {
                //                        val domain3 = domain2.visitNow(index)
                //                        println("nextEdge : " + domain3.nextEdge)
                //                        deepThrow(domain3.updated(index, tail), Some(index))
                //                    };
                //                }
                //                println("" + index * 100 / nodeCount)
            }

            //            deepThrow(List(List(1, 2), List(2, 3), List(3, 4)), 0, Nil)
            val prunedDomain = domain.pruneDomain // pruneDomain(domain).map(_.toList).toList
            deepThrow(prunedDomain, prunedDomain.nextEdge)

            solution.colors
        }


        val prunedDomain = initDomain.updated(initDomain.nextEdge.get, List(initDomain(initDomain.nextEdge.get).head)).pruneDomain
        Console.err.println("ToTest = " + initDomain.solutionsToTest)
        Console.err.println("ToTest = " + prunedDomain.solutionsToTest)

        colorsSolution(prunedDomain)
        //        prunedDomain.colors()
    }

    /**
     * Test constraints and return edges who are in errors.
     *
     * @param colors
     * @param edges
     * @return
     */
    def testingAgainsConstrains(colors: Seq[Int], edges: Seq[Edge]): Seq[Edge] = {
        if (colors.isEmpty) edges
        else edges.toStream.filter(e => colors(e.n1) == colors(e.n2))
    }

    def numberColors(colors: Seq[Color]): Int = {
        val uniqueColors = colors.toSet
        uniqueColors.size
    }

    /**
     * Read the instance, depthFirstBoundingBranch it, and print the solution in the standard output
     */
    def solve(args: Seq[String]) {
        var fileName: String = null
        for (arg <- args) {
            fileName = arg
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
        val nodeCount: Int = Integer.parseInt(firstLine(0))
        val edgeCount: Int = Integer.parseInt(firstLine(1))

        val edges = lines.slice(1, edgeCount + 1) map (line => {
            val parts: Array[String] = line.split("\\s+")

            Edge(java.lang.Integer.parseInt(parts(0)), java.lang.Integer.parseInt(parts(1)))
        })

        //        val sortedEdges = edges.filterNot(p => edges.count(e => (e.n1 == p.n1 && e.n2 == p.n2) || (e.n1 == p.n2 && e.n2 == p.n1)) > 1)

        val sortedEdges = edges.sortBy(_.n2)
        // So...
        var resultNO = simpleConstraintProgramming(sortedEdges, nodeCount)

        println(resultNO)
        //        val result = resultNO
        var cont = true
        do {
            Console.err.println(s"resultNO = $resultNO")
            val result = time(simpleConstraintProgramming4(Edges(edges), nodeCount, numberColors(resultNO) - 1))
            cont = testingAgainsConstrains(result, edges).isEmpty
            if (cont) {
                resultNO = result
                cont = true
            }
        } while (cont)

        Console.println(numberColors(resultNO) + " " + 0)
        resultNO.foreach(i => Console.print(i + " "))
        Console.println("")
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