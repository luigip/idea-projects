// 1. Write a code snippet that sets a to a Seq of n random integers between 0
// (inclusive) and n (exclusive).


// 2. Write a method that swaps adjacent elements of a Seq of integers. For example,
// Seq(1, 2, 3, 4, 5) becomes Seq(2, 1, 4, 3, 5).
// Hint: [grouped] [reverse] [map]


// 3. Repeat the preceding assignment, but produce a new Seq with the swapped
// values. Use for/yield.
// Hint: [grouped] again!


// 4. Given a Seq of integers, produce a new Seq that contains all positive
// values of the original Seq, in their original order, followed by all values that
// are zero or negative, in their original order.
// [partition]


// 5. How do you compute the average of an Seq[Double]?


// 6. How do you rearrange the elements of a Seq[Int] so that they appear in
// reverse sorted order?
// Do it using [reverse]
// Then using a method [sortWith / sortBy ?]
// Then again, using a custom Ordering (hint: supplied explicitly)


// 7. Write a function that takes a Seq and returns counts on each element
    def counts[T](xs: Seq[T]): Map[T,Int] = ???


// 8. Make a Seq of all time zones returned by java.util.TimeZone.getAvailableIDs
// that are in America. Strip off the "America/" prefix and sort the result.
// [filter] [map] [dropWhile]
