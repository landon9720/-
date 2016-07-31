package kuhn

import scala.collection.mutable.ArrayBuffer

case class Scale(steps: String, root: Int) {
  val ranks = {
    var i = 0
    val r = ArrayBuffer(i)
    steps.toUpperCase.foreach {
      case 'W' ⇒
        i += 2
        r += i
      case 'H' ⇒
        i += 1
        r += i
    }
    r.toArray
  }
  def apply(n: NoteOfScale): Note = {
    var rank = n.value
    var octave = 0
    while (rank < 0) {
      rank += ranks.size
      octave -= 1
    }
    while (rank >= ranks.size) {
      rank -= ranks.size
      octave += 1
    }
    Note(n.time, root + ranks(rank) + octave * 12 + n.accidental, n.duration, n.attack, n.release)
  }
}
object Scale {
  def MajorScale(root: Int) = Scale("WWHWWW", root)
  def MinorScale(root: Int) = Scale("WHWWHW", root)
  val C = 0
  val Cs = 1
  val D = 2
  val Ds = 3
  val E = 4
  val F = 5
  val Fs = 6
  val G = 7
  val Gs = 8
  val A = 9
  val As = 10
  val B = 11
}


