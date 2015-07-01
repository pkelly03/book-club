package paul.impatient.exercises

object Chapter12 {

  def exercise1_values(f: (Int) => Int, rangeStart: Int, rangeEnd: Int): List[(Int, Int)] =
    (rangeStart to rangeEnd).toList.map { idx => (idx, f(idx)) }

  def exercise2_largest_element_in_array_reduceLeft(arr: Array[Int]): Int = ???

  def exercise3_implement_factorial_function_using_to_and_reduceLeft_without_loop_or_recursion(): Int = ???
}
