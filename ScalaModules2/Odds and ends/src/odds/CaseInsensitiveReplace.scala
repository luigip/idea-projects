package odds

import scala.annotation.tailrec


object CaseInsensitiveReplace extends App {

  // replace all, case insensitive
  def replace(source: String, target: String, replacement: String): String = {
    
    // initialize the builder to sufficient size to reduce chance of needing to grow
    val out = new StringBuilder(source.size * 2)
    // last index we need to check for match
    val lastIdx = source.length - target.length
    // simple optimization
    val targetLower = target.toLowerCase
    

    @tailrec
    def matches(idx: Int, offset: Int): Boolean =
      if (offset >= target.length) 
        true
      else if(targetLower.charAt(offset) == source.charAt(idx + offset).toLower)
        matches(idx, offset + 1)
      else false
    
    @tailrec
    def search(idx: Int): Unit =
      if (idx > lastIdx)
        out.append(source.substring(idx))
      else if (matches(idx, 0)) {
        out.append(replacement)
        search(idx + target.length)
      }
      else {
        out.append(source.charAt(idx))
        search(idx + 1)
      }

    search(0)
    out.toString()
  }
  
  val test = "abcDefhIdeFdeDEFab"
  val res = replace(test,"def","XXXX")
  
  println(res) // abcXXXXhIXXXXdeXXXXab
               // abcXXXXhIXXXXdeXXXXab
  
}