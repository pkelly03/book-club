package ctci.chapter1

import scala.annotation.tailrec

object Exercise2 {

  def cStyleString(s:String): Array[Char] = nullTerminateArray(s.toCharArray)

  def nullTerminateArray(cs:Array[Char]) : Array[Char] = cs ++ Array[Char]('\u0000')

  object Paul{}

  object Ben {

    def reverseString_usingReverse(input: Array[Char]): Array[Char] = {
      nullTerminateArray((input.reverse map identity) tail)
    }

    def reverseString_usingRecurssion(input: Array[Char]): Array[Char] = {

      @tailrec
      def copy(c:Array[Char], result:Array[Char], idx:Int): Array[Char] = {
        if(idx >= c.length -1) result
        else {
          result(Math max(0,c.length - 2 - idx)) = c(idx)
          copy(c,result,idx+1)
        }
      }

      copy(input, new Array[Char](input.length), 0)
    }
  }
}
