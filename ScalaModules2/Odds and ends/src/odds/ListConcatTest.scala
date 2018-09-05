package odds

/**
 * Created by Rhys on 21/08/2015.
 */
// Benchmark is a trait that has been removed from recent Scala versions, but was very handy...

object ListConcatTest extends testingRJ.Benchmark {

  val longList = (1 to 100).toList
  val medList = (1 to 30).toList
  val shortList = (1 to 3).toList
  var result: List[Int] = _

  def run(): Unit = {
    // times are in ms for 100k trials

    //result = longList ::: shortList // 261
    //result = longList ++ shortList // 261

    //result = medList ::: medList ::: medList //156
    //result = medList ++ medList ++ medList //242

    //result = medList ::: medList  // 78
    //result = medList ++ medList // 80

    //result = shortList ::: shortList ::: longList // 18
    //result = shortList ++ shortList ++ longList // 32

    //result = longList ::: shortList ::: shortList // 284
    //result = longList ++ shortList ++ shortList // 553
    //result = List(longList, shortList, shortList).flatten // 121
  }

}
