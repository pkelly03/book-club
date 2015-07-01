package paul.impatient.exercises

object Chapter12 {

  def exercise1_values(f: (Int) => Int, rangeStart: Int, rangeEnd: Int): List[(Int, Int)] =
    (rangeStart to rangeEnd).toList.map { idx => (idx, f(idx)) }

}
