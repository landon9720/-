package kuhn

import org.junit.Test
import org.junit.Assert._
import ƒ._

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.CharSequenceReader

class ƒTest {
  @Test
  def test1 = {
    println(Abc.parse("CDEFGABcdefgab", MajorScale(0)))
  }
}

object Abc extends scala.util.parsing.combinator.Parsers {

  def parse(in: String, scale: Scale): Sequence[NoteOfScale] = {
    val items = sequence(new CharSequenceReader(in)).get
    var time = 0
    var result = List.empty[NoteOfScale]
    for (item ← items) yield {
      item match {
        case v: Value ⇒
          if (!v.rest) {
            result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = v.d, accidental = v.a)
          }
          time += v.duration.map(_.value).getOrElse(1*beats)
        case Chord(values) ⇒
          values foreach { v ⇒
            if (!v.rest) {
              result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = v.d, accidental = v.a)
            }
          }
          time += values.maxBy(_.d).d
      }
    }
    NoteOfScaleSequence(result:_*)
  }

  private def abcValueToNoteOfScaleRank(v: Value, scale: Scale): Int = {
    val letters = ArrayBuffer('C', 'D', 'E', 'F', 'G', 'A', 'B')
    val inputLetter = letters.indexWhere(_ == v.v)
    val scaleLetter = letters.indexWhere(_ == ArrayBuffer("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")(scale.root % 12).charAt(0))
    val degree = if (inputLetter >= scaleLetter) inputLetter - scaleLetter else (inputLetter + scale.ranks.size) - scaleLetter
    val noteValue = degree + scale.ranks.size * v.o
    noteValue
  }

  trait Item
  case class Value(accidentals: List[Accidental], value: Char, octaveModifiers: List[OctaveModifier], duration: Option[Duration]) extends Item {
    def a = accidentals.map(_.value).sum
    def rest = "ZXzx" contains value
    def v = value.toUpper
    def o = octaveModifiers.map(_.value).sum + (if (value.isLower) 1 else 0)
    def d = duration.map(_.value).getOrElse(1*beats)
  }
  case class Chord(values: List[Value]) extends Item
  case class Accidental(value: Int)
  case class OctaveModifier(value: Int)
  case class Duration(numerator: Int, denominator: Int) {
    def value = (numerator * beats) / (denominator * beats)
  }
  def sequence = rep1(value)
  def value = note | chord
  def note = rep(accidental) ~ v ~ rep(octaveModifier) ~ opt(duration) ^^ {
    case p ~ v ~ s ~ d ⇒ Value(p, v, s, d)
  }
  def chord = '[' ~> rep1(note) <~ ']' ^^ { case notes ⇒ Chord(notes) }
  def accidental = acceptMatch("accidental", {
    case '^' ⇒ Accidental(+1)
    case '=' ⇒ Accidental(0)
    case '_' ⇒ Accidental(-1)
  })
  def v = acceptMatch("value", {
    case c if "GABCDEFgabcdefZXzx" contains c ⇒ c
  })
  def octaveModifier = acceptMatch("octave modifier", {
    case ''' ⇒ OctaveModifier(+1)
    case ',' ⇒ OctaveModifier(-1)
  })
  def duration = digit ~ opt(accept('/') ~ opt(digit)) ^^ {
    case n ~ None ⇒ Duration(n, 1)
    case n ~ Some('/' ~ None) ⇒ Duration(n.asDigit, 2)
    case n ~ Some('/' ~ Some(d)) ⇒ Duration(n.asDigit, d.asDigit)
  }
  def digit = acceptMatch("digit", {
    case c if "0123456789" contains c ⇒ c
  })

  override type Elem = Char
}