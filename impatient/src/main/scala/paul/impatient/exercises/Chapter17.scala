package paul.impatient.exercises

object Chapter17 extends App {

  def getMiddle[T](a: Array[T]) = a(a.length / 2)

  val f = getMiddle[String] _

  val res = f(Array("aa","bb","cc","dd"))
  println(res)

  class Pair[T <: Comparable[T]](val first: T, val second: T) {
    def smaller = if(first.compareTo(second) < 0) first else second
  }

  val x = new Pair("aa", "bb").smaller
  println(x)

}
