package advent

object Day7 extends App {
  val SizeLimit = 100000

  def solve(lines: Seq[String]): Int = {
    var i = 0
    var total = 0

    def inner(): Int = {
      var sizes = 0

      while (i < lines.size) {
        val line = lines(i)

        if (line == "$ ls") {
          i += 1

          while (i < lines.size && !lines(i).startsWith("$")) {
            // skip dirs
            if (i < lines.size && lines(i).startsWith("dir ")) {
              i += 1
            } else if (i < lines.size && lines(i)(0).isDigit) {
              val num = lines(i).takeWhile(_.isDigit).toInt
              sizes += num
              i += 1
            }
          }
        } else if (line == "$ cd ..") {
          i += 1

          if (sizes < SizeLimit) total += sizes

          return sizes
        } else {
          i += 1
          sizes += inner()
        }
      }

      sizes
    }

    inner()

    total
  }

  val testInput = """$ cd /
  |$ ls
  |dir a
  |14848514 b.txt
  |8504156 c.dat
  |dir d
  |$ cd a
  |$ ls
  |dir e
  |29116 f
  |2557 g
  |62596 h.lst
  |$ cd e
  |$ ls
  |584 i
  |$ cd ..
  |$ cd ..
  |$ cd d
  |$ ls
  |4060174 j
  |8033020 d.log
  |5626152 d.ext
  |7214296 k""".stripMargin.split("\n")

  println(solve(testInput))
}
