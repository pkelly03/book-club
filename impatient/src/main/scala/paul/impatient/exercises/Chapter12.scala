package paul.impatient.exercises

object Chapter12 {

  def exercise1_values(f: (Int) => Int, rangeStart: Int, rangeEnd: Int): List[(Int, Int)] =
    (rangeStart to rangeEnd).toList.map { idx => (idx, f(idx)) }

  def exercise2_largest_element_in_array_reduceLeft(arr: Array[Int]): Int =
    arr.reduceLeft(_ max _)

  def exercise3_factorial(num: Int): Int =
    List.tabulate(num)(n => n + 1).reverse.reduceLeft(_ * _)

  def exercise5_largest(f: (Int) => Int, range: Range): Int =
    range.reduceLeft((a,b)=> a max f(b))

  def exercise6_largestAt(f: (Int) => Int, range: Range): Int =
    range.reduceLeft((a,b)=>  if (f(a) > f(b)) a else b)

}
