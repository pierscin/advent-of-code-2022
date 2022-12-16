package advent

import scala.collection.mutable

object Day16 extends App {
  val testInput = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                    |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                    |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                    |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                    |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                    |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                    |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                    |Valve HH has flow rate=22; tunnel leads to valve GG
                    |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                    |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin
    .split("\n")

  case class Node(id: String, rate: Int, adj: List[String])
  def parseLine(s: String) = {
    val idPattern = "[A-Z]{2,}".r
    val ids = idPattern.findAllIn(s).toList
    val id = ids.head
    val connected = ids.tail

    val rate = s.substring(s.indexOf('=') + 1, s.indexOf(';')).toInt
    Node(id, rate, connected)
  }

  def part1(inputs: Seq[String]): Int = {
    val g = inputs.map(parseLine).map(node => node.id -> node).toMap

    val cache = mutable.Map.empty[(String, Set[String], Int), Int]

    def inner(cur: String, opened: Set[String], t: Int): Int = {
      if (t <= 0) return 0

      if (!cache.contains((cur, opened, t))) {
        val node = g(cur)
        var best = 0

        if (!opened.contains(cur)) {
          val totalReleased = (t - 1) * node.rate

          best = node.adj.flatMap { s =>
            Option.when(node.rate != 0)(
              totalReleased + inner(s, opened + node.id, t - 2)
            ) ++ List(inner(s, opened, t - 1))
          }.max
        } else {
          best = node.adj.map(s => inner(s, opened, t - 1)).max
        }

        cache += (cur, opened, t) -> best
      }

      cache((cur, opened, t))
    }

    inner("AA", Set.empty[String], 30)
  }

  println(part1(testInput))
  println(
    part1(
      scala.io.Source.fromResource("day16_in.txt").getLines().toList
    )
  )
}
