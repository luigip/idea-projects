package benchmarking

import scala.annotation.tailrec

object Bench extends testingRJ.Benchmark {

  val longText = "This is a test text containing something and SoMETHING else else!x" * 100
  
  val answer = longText.replaceAll("(?i)eLSE!", "someother..")

  def replaceAllCaseInsensitive(source: String, target: String, replacement: String) = {
    val replacementSize = replacement.size
    val sourceSize = source.size
    val targetSize = target.size
    val sizeDelta = replacementSize - targetSize
    val builder = new StringBuilder(source)
    val end = if(targetSize > 0) sourceSize - targetSize else sourceSize - 1
    val targetLower = target.toLowerCase
    
    
    @inline
    def findMatchAt(offset: Int): Boolean = {
      var i = 0
      while (i < targetSize) {
        if (source.charAt(i + offset).toLower != targetLower.charAt(i))
          return false
        i += 1
      }
      return true
    }

    @tailrec
    def loop(currentIdx: Int = 0, offset: Int = 0): String = {
      if (currentIdx <= end) {
        if (findMatchAt(currentIdx)) {
          builder.replace(currentIdx + offset, currentIdx + offset + targetSize, replacement)
          loop(currentIdx + targetSize, offset + sizeDelta)
        } else
          loop(currentIdx + 1, offset)
      } else {
        builder.result()
      }
    }

    loop()
  }
  
  //def test = replaceAllCaseInsensitive(longText,"eLSE!","someother..")  //667   //420
  def test = odds.CaseInsensitiveReplace.replace(longText,"eLSE!","someother..")//34  //367
  //def test = longText.replaceAll("(?i)eLSE!", "someother..")//57  //388
  
  def run() {
    test
  }
//  
//  println(test)
//  println(answer)
  assert(test == answer, "answer incorrect!")
  
  
}