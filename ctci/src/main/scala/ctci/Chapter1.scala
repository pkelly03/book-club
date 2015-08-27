package ctci

object Chapter1 {

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
