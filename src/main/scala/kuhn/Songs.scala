package kuhn

import kuhn.Monad._
import kuhn.Notes._
import kuhn.Song._
import kuhn.ƒ._

object Song1 extends Song {
  def part(a: Int, b: Int) = n(a+0, a+b) N
  val song = {
    StartOfNotes >>
      {
        (for {
          a ← 0 to 12
          b ← 0 to 12
        } yield part(a, b-a)) reduce { _ > _ }
      }
  }
}

object Song2 extends Song {
  val song = {
    StartOfNotes >
      (n(C5) * 1 N) >
      (n(D5) * 2 N) >
      (n(E5) * 3 N) >
      (n(F5) * 4 N) >
      (n(G5) * 5 N) >
      (n(A6) * 6 N) >
      (n(B6) * 7 N) >
      (n(C6) * 8 N)
  }
}

object Song3 extends Song {
  implicit val x = NoteValue
  val part1 = b(C4) > b(C4, E4) > b(C4, E4, G4, C5)
  val song =
    StartOfNotes >
      part1 >
      (part1 up 2) >
      (part1 up 4) >
      (part1 up 2) ===
        ((part1 down 12) ** 4)
}

object Song4 extends Song {
  implicit val x = NoteValue
  val stepsequence  = Seq(0, 5, 10%7, 15%7, 20%7, 25%7)

  def p: Monad[NoteOfScale, NoteOfScale] = (bb(0,5,5,5) > bb(0,4,4,4) > bb(0, 1)) ** 2
  val notes: Monad[NoteOfScale, NoteOfScale] = stepsequence.map(p up).reduce(_>_)
  val backup: Monad[NoteOfScale, NoteOfScale] = stepsequence.map(p down 12 up).reduce(_>_)

  val song = StartOfNotes >
    ((StartOfNoteOfScales >> notes) >> NoteOfScaleToNotes(CMajor)) === ((StartOfNoteOfScales >> backup) >> NoteOfScaleToNotes(CMajor) down 3)
}

object Song5 extends Song {
  implicit val x = NoteAttack
  val song = StartOfNotes >
    ((b(0, 1, 2, 3) * 5) vmap ConstValMod(30)) >
    ((b(0, 1, 2, 3) * 5) vmap ConstValMod(40)) >
    ((b(0, 1, 2, 3) * 5) vmap ConstValMod(50)) >
    ((b(0, 1, 2, 3) * 5) vmap ConstValMod(60)) >
    ((b(0, 1, 2, 3) * 5) vmap ConstValMod(70))
}

trait TemplateSong extends Song {
  override def song: Monad[Note, Note] = StartOfNotes
}

object Song6 extends TemplateSong {
  val scale = MinorScale(A5)
  val noteOfScaleSequences = Map(
    "melody" → "0011 2___ 2211 0____",
    "base"   → "0 2 21 0"
  )
  val start = ("melody", "base")
}