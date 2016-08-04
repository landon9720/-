package kuhn

import kuhn.Monad._
import kuhn.Scale._
import kuhn.Song._
import kuhn.ƒ._

import scala.collection.mutable
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

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

case class MatrixNoteGenerator[T <: Event[T]](matrix: Matrix) extends Monad[T, NoteOfScale] {
  def apply(ignored: Sequence[T]): Sequence[NoteOfScale] = {
    val result = new mutable.ListBuffer[NoteOfScale]
    matrix foreach { (x, y, value) ⇒
      if (value != -1) {
        val attack = 128 productWithRatio(value, 16)
        result += NoteOfScale(x*beats, y, duration = 1*beats)
      }
    }
    val duration = matrix.width
    NoteOfScaleSequence(result.toList, duration)
  }
}

case class ChordsToNoteOfScales(matrix: Matrix) extends Monad[Chord, NoteOfScale] {
  def apply(input: Sequence[Chord]): Sequence[NoteOfScale] = {
    val result = new mutable.ListBuffer[NoteOfScale]
    for (Chord(time, root, ranks) ← input.events) {
      matrix foreach { (colOffset, rowRank, colValue) ⇒
        if (colValue != -1) {
          var rowRank1 = rowRank
          var octaves = 0
          while (rowRank1 < 0) {
            rowRank1 += ranks.size
            octaves -= 1
          }
          while (rowRank1 >= ranks.size) {
            rowRank1 -= ranks.size
            octaves += 1
          }
          val attack = 128 productWithRatio(colValue, 16)
          val value = root + 7 * octaves + ranks(rowRank1)._1
          val accidental = ranks(rowRank1)._2
          result += NoteOfScale(time + colOffset*beats, value, duration = 1*beats, attack = attack, release = attack, accidental = accidental)
        }
      }
    }
    val duration = matrix.width
    NoteOfScaleSequence(result.toList, duration)
  }
}

case class Matrix(rows: Seq[Seq[Int]], offset: (Int, Int)) {
  assert(rows.tail.forall(_.size == rows.head.size))
  val height = rows.size
  val width = rows.head.size
  val (offsetX, offsetY) = offset
  val left = offsetX
  val right = left + width
  val top = offsetY
  val bottom = top + height
  def foreach(f: (Int, Int, Int) ⇒ Unit): Unit = {
    for {
      (row, rowIndex) ← rows.zipWithIndex
      (cell, colIndex) ← row.zipWithIndex
    } {
      f(colIndex + offsetX, rowIndex + offsetY, cell)
    }
  }
}

object Matrix {
  def apply(asString: String, offset: (Int, Int) = (0, 0)): Matrix = {
    val rows = asString.split("\n").filterNot(_.isEmpty)
    val len = rows.maxBy(_.size).size
    new Matrix(for (r ← rows) yield r.padTo(len, ' ').map(_.asDigit), offset)
  }
}

object ChordBuilder extends Parsers {
  def parse(in: String, time: Int, rankOfScale: Int, offset: Int = -1): Chord = {
    Chord(time, rankOfScale, phrase(values)(new CharSequenceReader(in)) match {
      case Success(values, _) ⇒ values map { case (a, b) ⇒ (a + offset, b) }
      case n: NoSuccess ⇒ sys.error(n.toString)
    })
  }
  def values = rep1sep(value, ',')
  def value = rankOfScale ~ accidental ^^ { case a ~ b ⇒ (a, b) }
  def rankOfScale = rep1(digit) ^^ { _.mkString.toInt }
  def digit = acceptMatch("digit", { case c if "0123456789" contains c ⇒ c })
  def accidental = sharp | flat | success(0)
  def sharp = accept('+') ^^ { _ ⇒ 1 }
  def flat = accept('-') ^^ { _ ⇒ -1 }
  override type Elem = Char
}

object Song14 extends Song {

  val matrix = Matrix("""
a
a
a
a """)

  var _t = 0
  def t = {
    val r = _t
    _t += matrix.width*beats
    r
  }

  def song =
    StartOfChords >> ChordPart(ChordSequence(
      ((0 to 7) map { ChordBuilder.parse("1,3,5,7-", t, _) }) ++
      ((0 to 7) map { ChordBuilder.parse("1,3,5,7", t, _) }) :_*
    )) >> ChordsToNoteOfScales(matrix) >> NoteOfScalesToNotes(scale)

  val scale = MajorScale(C)
}

object Song15 extends Song {

  val matrix = Matrix("""
a              a         a
 a
  a              99
   a                       99
    a
     a
      a
       a  a  a                               """)

  var _t = 0
  def t = {
    val r = _t
    _t += matrix.width*beats
    r
  }

  def song =
    StartOfNoteOfScales >> MatrixNoteGenerator(matrix) >> NoteOfScalesToNotes(scale)

  val scale = MajorScale(C)
}
