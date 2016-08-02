package kuhn

import kuhn.Monad._
import kuhn.Scale._
import kuhn.Song._
import kuhn.ƒ._

import scala.collection.mutable

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
    ((StartOfNoteOfScales >> notes) >> NoteOfScalesToNotes(MajorScale(C))) === ((StartOfNoteOfScales >> backup) >> NoteOfScalesToNotes(MajorScale(C)) down 3)
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
    StartOfNoteOfScales >> (s === (s up 7) === (s down 7)) >> NoteOfScalesToNotes(scale)
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
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(C)) >> NoteOfScalesToNotes(MajorScale(C))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(D)) >> NoteOfScalesToNotes(MajorScale(D))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(E)) >> NoteOfScalesToNotes(MajorScale(E))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(F)) >> NoteOfScalesToNotes(MajorScale(F))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(G)) >> NoteOfScalesToNotes(MajorScale(G))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(A)) >> NoteOfScalesToNotes(MajorScale(A))) >
    (StartOfNoteOfScales >> Abc.parse("CDEFGABc", MajorScale(B)) >> NoteOfScalesToNotes(MajorScale(B)))
  }
}

object Song8 extends Song {
  def song: Monad[Note, Note] = {
    val s = Abc.parse(abc, scale)
    StartOfNoteOfScales >> (s === (s up 7) === (s down 7)) >> NoteOfScalesToNotes(scale)
  }
  val abc = """
G2DGFDG2DdcBG2DGFD2GFDGFFGFDEB2B2BG/F/EFGFDEB2B2ABdG2DGFDG2DdcBG2DGFD2GFDGF
DE2GFABF2F2DE/F/GFABF2DBDG2d2cBGdcBGG2GDGBGA2DGBGAdgafdgabgg2fdcdBGG2DGBGAd
"""
  val scale = MinorScale(A)
}

object Song9 extends Song {
  def song = {
    val s = Abc.parse(abc, scale)
    StartOfNoteOfScales >> (s === (s up 7) === (s down 7)) >> NoteOfScalesToNotes(scale)
  }
  val abc = """
EEE2BEE2B2AFDEFDEGFEBEE2B2AcBEEDEEE2BEE2B2AFDEFDEGFEBEE2B2AcBEE2
e2g/f/ebeged2dfaafde2gebegedBAcBEE2e2g/f/ebeg/f/ed2dfa2fdefgefdeBd/c/BAcBEE2"""
  val scale = MinorScale(E)
}

object Song10 extends Song {
  def song = StartOfNoteOfScales >> Abc.parse(abc, scale) >> NoteOfScalesToNotes(scale)
  val abc = """[Ace2][ceg2][egb4]"""
  val scale = MajorScale(A)
}

object Song11 extends Song { // Brenda Stubbert's
  def song = StartOfNoteOfScales >> Abc.parse(abc, scale) >> NoteOfScalesToNotes(scale)
  val abc = """
B A/A/A BA GAAB A/A/A BA edde G2 BA BGGB c2 BA BGGB A/A/A BA GAAB A/A/A BA edda gedB GABd e2 dB eAA
B A/A/A BA GAAB A/A/A BA edde G2 BA BGGB c2 BA BGGB A/A/A BA GAAB A/A/A BA edda gedB GABd e2 dB eAA
B A/A/A a2 A/A/A g2 Aage ageg G2 BA BGGB c2 BA BGGB A/A/A a2 A/A/A g2 Aage agea gedB GABd e2 dB eAA
B A/A/A a2 A/A/A g2 Aage ageg G2 BA BGGB c2 BA BGGB A/A/A BA GAAB A/A/A BA edda gedB GABd e2 dB eAA
"""
  val scale = MinorScale(A)
}

object Song12 extends Song {
  def song = StartOfNoteOfScales >> Abc.parse(abc, scale) >> NoteOfScalesToNotes(scale)
  val abc = """ C2 x CC x C<C x C<<C x C<<<C x C<<<<C x C2 x CC x C>C x C>>C x C>>>C x C>>>>C """
  val scale = MajorScale(C)
}

object Song13 extends Song {
  def song = StartOfNoteOfScales >> Abc.parse(abc, scale) >> NoteOfScalesToNotes(scale)
  val abc = """
G>A |B2 (3ABA G2 (3FGF | E>FG>A B2 e>f | g>fe>d e>d (3Bcd | c>BA<G A2 (3FGA | (3BcB A2 (3GAG F>G | E>FG<A B2 e>f | g2 (3fed B2 (3fgf | e>E E>^D E2
G>A |B2 (3ABA G2 (3FGF | E>FG>A B2 e>f | g>fe>d e>d (3Bcd | c>BA<G A2 (3FGA | (3BcB A2 (3GAG F>G | E>FG<A B2 e>f | g2 (3fed B2 (3fgf | e>E E>^D E2
g>a |b2- b>g e2 (3fga | b>^ab>g e2 (3gfe | d3 e f3 g | a>f (3def a2 (3agf | e^def g>fg>a | b2 (3agf g>fe>f | g>fe<d B2 (3gfg | e2 E2 E2
g>a |b2- b>g e2 (3fga | b>^ab>g e2 (3gfe | d3 e f3 g | a>f (3def a2 (3agf | e^def g>fg>a | b2 (3agf g>fe>f | g>fe<d B2 (3gfg | e2 E2 E2
  """
  val scale = MinorScale(E)
}

object Song14 extends Song {

  case class ChordsToNoteOfScales(matrix: Matrix) extends Monad[Chord, NoteOfScale] {
    def apply(input: Sequence[Chord]): Sequence[NoteOfScale] = {
      val result = new mutable.ListBuffer[NoteOfScale]
      for (Chord(time, root, ranks) ← input.events) {
        for ((row, rowRank) ← matrix.rows.zipWithIndex) {
          for {
            (colValue, colOffset) ← row.zipWithIndex
            if colValue != -1
          } {
            result += NoteOfScale(time + colOffset*beats, root+ranks.apply(rowRank))
          }
        }
      }
      var duration = matrix.width
      NoteOfScaleSequence(result.toList, duration)
    }
  }

  case class Matrix(rows: Seq[Seq[Int]]) {
    assert(rows.tail.forall(_.size == rows.head.size))
    val height = rows.size
    val width = rows.head.size
  }

  object Matrix {
    def apply(asString: String): Matrix = {
      val rows = asString.split("\n").filterNot(_.size == 0)
      val len = rows.maxBy(_.size).size
      new Matrix((for (r ← rows) yield r.padTo(len, ' ').map(_.asDigit)))
    }
  }

  val matrix = Matrix("""
55
5 5
5  5
""")

  def Triad(time: Int, root: Int) = Chord(time*beats, root, Seq(1, 3, 5).map(_-1))

  var _t = 0
  def t = {
    val r = _t
    _t += 4
    r
  }

  def song =
    StartOfChords >> ChordPart(ChordSequence(
      ((0 to 6) ++ (7 to -6 by -1) ++ (-7 to -1)) map { Triad(t, _) } :_*
    )) >> ChordsToNoteOfScales(matrix) >> NoteOfScalesToNotes(scale)

  val scale = MajorScale(E)
}
