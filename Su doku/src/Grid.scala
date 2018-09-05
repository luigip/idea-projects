/**
 * Created by Rhys on 31 May 2015.
 */

import collection.mutable.{Set => MSet}

class Grid
  (val input: Vector[String]) {

  def result: Seq[String] = for {
    group <- rows} yield (for {
    cell <- group.cells
    charVal = if (cell.size == 1) (cell.head + 48).toChar else '.'
  } yield charVal).mkString

  type Cell = MSet[Int]

  class Group (
    // cells in this Group
    val cells: Seq[Cell]
    ) {
    // cells that each number could possibly fit in
    val valueMap: Map[Int, Seq[Cell]] =
      (1 to 8).map(i => (i, Seq(cells:_*))).toMap

  }

  val rows: Seq[Group] = {
    def charToCell(c: Char): Cell =
      c.asDigit match {
        case -1 => MSet(1 to 9:_*)
        case x => MSet(x)
      }
    input.map(str => new Group(str.map(charToCell)))
  }

  val cols: Seq[Group]=
    for {idx <- 0 to 8 } yield
    new Group(for {col <- 0 to 8 } yield rows(idx).cells(col))

  val subgrids: Seq[Group] =
    for {x <- 0 to 2
         y <- 0 to 2} yield {
      val cells =
        for {i <- 0 to 2
             j <- 0 to 2
             cell = rows(y * 3 + j).cells(x * 3 + i)
        } yield cell
      new Group(cells)
    }

  val superGroups = List(rows, cols, subgrids)

  def solve(): Unit = {
    //debug
    superGroups flatMap identity foreach {g => println(g.valueMap)
                                               println(g.cells)}
    println("end debug")

    var changes = 0
    superGroups foreach {sg =>
      sg foreach {g =>
        // eliminate number possibilities in each cell due to number being used elsewhere in group
        g.cells foreach { c =>
          c.size match {
            case 0 => println("ERROR: No possibilities for cell")
                      println("Group cells: " + g.cells)
                      println("Group valueMap" + g.valueMap)
                      result foreach println
                      sys.exit()

            // cell is filled in, so eliminate possibilities in other cells in group
            case 1 => g.cells foreach { c1 =>
              if (c1 ne c) {
                if(c1(c.head)){
                  c1 -= c.head
                  changes += 1
                }
              }
            }
            case _ =>
          }
        }
        // insert numbers if they can only go in n places
        println(g.valueMap)
        g.valueMap foreach { case (i, cs) =>
          // cs is the set of cells where i fits
          // if only 1 cell is possible,
          if (cs.size == 1) {
            // eliminate all other possibilities from that cell
            println("BOOM!!")
            println((i,cs))
            cs.head.retain(_ == i)
            println((i,cs))
            changes += 1
          }
        }
      }
    }
    println("number of changes: " + changes)
    if (changes > 0) solve()
  }


}


















































