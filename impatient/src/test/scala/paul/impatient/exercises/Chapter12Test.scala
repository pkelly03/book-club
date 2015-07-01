package paul.impatient.exercises

import org.scalatest.{FunSpec, Matchers}
import paul.impatient.exercises.Chapter12._

class Chapter12Test extends FunSpec with Matchers {

  describe("exercise1_values") {
    /*
      Write a function values(fun: (Int) => Int, low: Int, high: Int) that yields a collection of '
      function inputs and outputs in a given range. For example, values(x => x * x, -5, 5) should produce
      a collection of pairs (-5, 25), (-4, 16), (-3, 9), . . ., (5, 25).”
     */
    it("should produce (-2, -4), (-1, -2) (0,0), (1,2), (2,4) when input is values(x => x + x, -2, 2)") {
      exercise1_values((x:Int) => x + x, -2, 2) should be(List((-2, -4), (-1, -2), (0,0), (1,2), (2,4)))
      exercise1_values((x:Int) => x * x, -2, 2) should be(List((-2,4), (-1,1), (0,0), (1,1), (2,4)))
    }
  }

  describe("exercise2_largest_element_in_array_reduceLeft") {
    it("should find the largest element in an array with reduceLeft") {
      exercise2_largest_element_in_array_reduceLeft(Array(2, 4, 6, 1)) should be(6)
      exercise2_largest_element_in_array_reduceLeft(Array(20, 4, 6, 1)) should be(20)
    }
  }

  describe("exercise3_implement_factorial_function_using_to_and_reduceLeft_without_loop_or_recursion") {
    it("should ...") {
      exercise3_implement_factorial_function_using_to_and_reduceLeft_without_loop_or_recursion
    }
  }
}
