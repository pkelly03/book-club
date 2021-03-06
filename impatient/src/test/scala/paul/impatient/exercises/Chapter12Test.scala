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
      // exercise3_factorial(4) should be 24 ( 4 x 3 x 2 x 1 )
      // so exercise3_factorial(4) should be 120 (5 x 4 x 3 x 2 x 1)
      exercise3_factorial(4) should be (24)
      exercise3_factorial(5) should be (120)
    }
  }

  describe("exercise5_largest_yields_largest_value_of_function_within_sequence_of_inputs") {
    it("should return the largest sequence in a range without using loops or recursion") {
      exercise5_largest(x => 10 * x, 1 to 10) should be (100)
    }
  }

  describe("exercise6_largestAt_yields_largest_index_within_sequence_of_inputs") {
    it("should return the index where the largest value is") {
      exercise6_largestAt(x => 10 * x, 1 to 10) should be (10)
    }
  }

  describe("exercise7_function_on_pairs") {
    it("should perform a function on a set of pairs") {
      val pairs = (1 to 10) zip (11 to 20)
      exercise7_function_on_pairs() should be (10)
    }
  }

}
