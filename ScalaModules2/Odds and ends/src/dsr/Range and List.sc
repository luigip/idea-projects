
// Ranges
//1.	Create a Range from 5 to 15
//  a.	Using methods on Range object


//  b.	Using a method on RichInt


//2.	Create a Range of odd numbers from 1 to 21


//3.	What’s the memory footprint of Range?

//4.	Make a Range 1 to 10, and Use [map] to map elements to their squares.


//    What is the result type? Why?


// Lists

val nums = List(1, 2, 3, 4, 5)

var sum = 0
for (n <- nums) {
  sum += n
}
sum

// Write a function (Int, Int) => Int, which adds two ints together


// Rewrite "sum" using [foldLeft] and your function. Folding is a functional loop!
//val sum2 = ???


// Inline your function (i.e. use an anonymous function), and use placeholders


// Check out the method [/:] - it's a symbolic version of foldLeft.
// Becuase it ends in ":", it's right-associative when you use it as an operator. Do so.


// What's the difference between foldLeft and reduceLeft?
// Write the sum method using reduceLeft.


val strs = List("1", "2", "3", "4", "5")

// Convert this to a List of Ints
// [map]


val strs2 = List("1", "two", "3", "four", "five")
// Use [flatten] to convert this list to a list of Chars


// Use [map] to map the list to their corresponding Int value, using the `toInt` method
// (one of Scala's enhancements - see StringOps)
//
// ... trick question! Obviously not possible in some cases, since for "two", an Exception
// will be thrown. So use try / catch to map to a container type instead, which can contain
// zero or 1 element
// a) use List[Int]


// b) use Option[Int] (hint: use its two sub-types)


// Now flatten each of these... hopefully we now have List(1, 3)


// You can combine these actions using [flatMap]. Which is more appropriate to use for the
// container: List or Option? Use the better one, and check the answer is the same as above.

