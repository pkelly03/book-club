package ctci

object Chapter1 {
  def exercise1_1(input:String):Boolean = {

    input.exists { el =>
      val charCounter = input.foldLeft[Option[Pair[Char, Int]]](None)((acc, currentElement) =>
        if (currentElement.equals(el)) acc.map(p => el -> (p._2 + 1)) orElse Option(el -> 1) else acc
      )

      !charCounter.exists(acc => acc._2 > 1)
    }
  }
}
