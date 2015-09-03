package ctci.chapter1

import scala.annotation.tailrec

object Exercise4 {

  object Paul {}

  object Ben {

    def areAnagrams_usingSort(as: String, bs: String): Boolean = {
      as.length == bs.length && as.sortWith(_ < _).equals(bs.sortWith(_ < _))
    }

    def areAnagrams_usingGroupBy(as: String, bs: String): Boolean = {
      as.groupBy(identity).equals(bs.groupBy(identity))
    }

    def areAnagrams_usingRecursion(as: String, bs: String): Boolean = {

      @tailrec
      def isAnagram(ls:List[Char], lls:List[Char]): Boolean = ls match {
        case Nil => lls.isEmpty
        case _ if lls.isEmpty => true
        case x :: xs => isAnagram(xs, lls diff List(x))
      }

      isAnagram(as toList, bs toList)
    }
  }
}
