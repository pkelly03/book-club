package ctci.chapter1

import scala.annotation.tailrec

object Exercise3 {

  object Paul {}

  object Ben {

    def removeDuplicates_usingDistinct(s: String) = s.distinct

    def removeDuplicates_usingRecursion(s: String) = {

      def removeElement(position: Int, chars: Array[Char]): Array[Char] = {
        val sp = chars.splitAt(position)
        sp._1 ++ sp._2.drop(1)
      }

      @tailrec
      def dedupe(position: Int, toRemove: Char, array: Array[Char]): Array[Char] = {
        if (position >= array.length) array
        else {
          if (array(position).equals(toRemove)) dedupe(position, toRemove, removeElement(position, array))
          else dedupe(position + 1, toRemove, array)
        }
      }

      def iterate(array: Array[Char], position: Int): Array[Char] = {
        if (position >= array.length) array
        else iterate(dedupe(position + 1, array(position), array), position + 1)
      }

      new String(iterate(s.toCharArray, 0))
    }
  }
}
