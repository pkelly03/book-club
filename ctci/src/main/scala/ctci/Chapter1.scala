package ctci

object Chapter1 extends App {

  object Paul {

    def exercise1_usingDistinct(input: String): Boolean =
      input.toList.distinct == input.toList

    def exercise1_usingCollect(input: String): Boolean =
      input.toList.groupBy(identity).collect {
        case (x,ys) if ys.size > 1 => x
      }.isEmpty

    def exercise1_usingFoldLeftWithImmutableSet(input: String): Boolean =
      input.foldLeft((Set.empty[Char], Set.empty[Char])) { case ((inputSet, duplicateSet), currentChar) =>
        if (inputSet(currentChar)) (inputSet, duplicateSet + currentChar)
        else (inputSet + currentChar, duplicateSet)
      }._2.isEmpty

    def exercise1_usingFoldLeftWithOneImmutableSet(input: String): Boolean =
      input.foldLeft((Set.empty[Char])) { case (inputSet, currentChar) =>
        inputSet + currentChar
      }.size == input.length

    def exercise1_usingTailRecursion(input: String): Boolean = ???
  }

  object Ben {
    def exercise1_1(input: String): Boolean = {

      input.exists { el =>
        val charCounter = input.foldLeft[Option[(Char, Int)]](None)((acc, currentElement) =>
          if (currentElement.equals(el)) acc.map(p => el -> (p._2 + 1)) orElse Some(el -> 1) else acc
        )

        !charCounter.exists(acc => acc._2 > 1)
      }
    }

    def exercise1_1b(input: String): Boolean = {

      input.groupBy(identity).collect {
        case (p,grouped) if grouped.length > 1 => p
      }.isEmpty
    }
  }
}
