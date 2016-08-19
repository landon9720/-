package kuhn

trait Scale extends Monad

case class RankScale(ranks: List[Int]) extends Scale {
  override def apply(input: Sequence): Sequence =
    Sequence(for (value ← input.values) yield {
      var rank = value.value
      var octave = value.octave
      while (rank < 0) {
        rank += ranks.size
        octave -= 1
      }
      while (rank >= ranks.size) {
        rank -= ranks.size
        octave += 1
      }
      value.copy(value = ranks(rank) + value.accidental, octave = octave)
    })
}