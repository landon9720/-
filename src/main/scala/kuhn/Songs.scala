package kuhn

import kuhn.Monad._
import kuhn.Song._
import kuhn.ƒ._
import kuhn.Scale._

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
      (n(C) * 1 N) >
      (n(D) * 2 N) >
      (n(E) * 3 N) >
      (n(F) * 4 N) >
      (n(G) * 5 N) >
      (n(A) * 6 N) >
      (n(B) * 7 N) >
      (n(C) * 8 N)
  }
}

object Song3 extends Song {
  implicit val x = NoteValue
  val part1 = b(C) > b(C, E) > b(C, E, G, C)
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
    ((StartOfNoteOfScales >> notes) >> NoteOfScaleToNotes(MajorScale(C))) === ((StartOfNoteOfScales >> backup) >> NoteOfScaleToNotes(MajorScale(C)) down 3)
}

object Song5 extends Song {
  implicit val x = NoteAttack
  val song = StartOfNotes >
    ((b(0, 1, 2, 3) * 5) map ConstValMod(NoteAttack, 30)) >
    ((b(0, 1, 2, 3) * 5) map ConstValMod(NoteAttack, 40)) >
    ((b(0, 1, 2, 3) * 5) map ConstValMod(NoteAttack, 50)) >
    ((b(0, 1, 2, 3) * 5) map ConstValMod(NoteAttack, 60)) >
    ((b(0, 1, 2, 3) * 5) map ConstValMod(NoteAttack, 70))
}

trait AbcSong extends Song {
  def abc: String
  def scale: Scale

}

object Song6 extends Song {
  def song: Monad[Note, Note] = {
    val s = Abc.parse(abc, scale)
    StartOfNoteOfScales >> (s === (s up 7) === (s down 7)) >> NoteOfScaleToNotes(scale)
  }
  val abc = """
B|A/A/A (BA) GAAB|A/A/A (BA) edde|G2 (BA) BGGB| c2 (BA) BGGB| A/A/A (BA) GAAB|A/A/A (BA) edda|gedB GABd|e2 dB eAA
B|A/A/A (BA) GAAB|A/A/A (BA) edde|G2 (BA) BGGB| c2 (BA) BGGB| A/A/A (BA) GAAB|A/A/A (BA) edda|gedB GABd|e2 dB eAA
B|A/A/A a2 A/A/A g2| Aage ageg|G2 (BA) BGGB| c2 (BA) BGGB|A/A/A a2 A/A/A g2| Aage agea| gedB GABd|e2 dB eAA
B|A/A/A a2 A/A/A g2| Aage ageg|G2 (BA) BGGB| c2 (BA) BGGB|A/A/A (BA) GAAB|A/A/A (BA) edda| gedB GABd|e2 dB eAA
  """
  val scale = MinorScale(A)
}

object Song7 extends Song {
  override def song: Monad[Note, Note] = {
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(C)) >> NoteOfScaleToNotes(MajorScale(C))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(D)) >> NoteOfScaleToNotes(MajorScale(D))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(E)) >> NoteOfScaleToNotes(MajorScale(E))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(F)) >> NoteOfScaleToNotes(MajorScale(F))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(G)) >> NoteOfScaleToNotes(MajorScale(G))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(A)) >> NoteOfScaleToNotes(MajorScale(A))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(B)) >> NoteOfScaleToNotes(MajorScale(B)))
  }
}

object Song8 extends Song {
  def song: Monad[Note, Note] = {
    val s = Abc.parse(abc, scale)
    StartOfNoteOfScales >> (s === (s up 7) === (s down 7)) >> NoteOfScaleToNotes(scale)
  }
  val abc = """
G2DGFDG2DdcBG2DGFD2GFDGFFGFDEB2B2BG/F/EFGFDEB2B2ABdG2DGFDG2DdcBG2DGFD2GFDGF
DE2GFABF2F2DE/F/GFABF2DBDG2d2cBGdcBGG2GDGBGA2DGBGAdgafdgabgg2fdcdBGG2DGBGAd
"""
  val scale = MinorScale(A)
}

object Song9 extends Song {
  def song: Monad[Note, Note] = {
    val s = Abc.parse(abc, scale)
    StartOfNoteOfScales >> (s === (s up 7) === (s down 7)) >> NoteOfScaleToNotes(scale)
  }
  val abc = """
EEE2BEE2B2AFDEFDEGFEBEE2B2AcBEEDEEE2BEE2B2AFDEFDEGFEBEE2B2AcBEE2
e2g/f/ebeged2dfaafde2gebegedBAcBEE2e2g/f/ebeg/f/ed2dfa2fdefgefdeBd/c/BAcBEE2
            """
  val scale = MinorScale(E)
}