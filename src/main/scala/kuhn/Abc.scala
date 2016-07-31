package kuhn

import kuhn.ƒ._

import scala.util.parsing.input.CharSequenceReader

object Abc extends scala.util.parsing.combinator.Parsers {

  def parse[T <: Event[T]](in: String, scale: Scale): NoteOfScalePart[NoteOfScale] = {
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
        case _: Ignored ⇒
          // ignored
      }
    }
    val s = result mkString "\n"
    NoteOfScalePart[NoteOfScale](NoteOfScaleSequence(result, time))
  }

  private def abcValueToNoteOfScaleRank(v: Value, scale: Scale): Int = {
    val letters = Array('C', 'D', 'E', 'F', 'G', 'A', 'B')
    val inputLetter = letters.indexWhere(_ == v.v)
    assert(inputLetter != -1)
    val scaleLetter = letters.indexWhere(_ == Array("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B").apply(scale.root % 12).charAt(0))
    assert(scaleLetter != -1)
    val degree =
      if (inputLetter >= scaleLetter) inputLetter - scaleLetter
      else (inputLetter + scale.ranks.size) - scaleLetter - scale.ranks.size
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
  case class Ignored() extends Item
  case class Accidental(value: Int)
  case class OctaveModifier(value: Int)
  case class Duration(numerator: Int, denominator: Int) {
    val value = beats / denominator * numerator
  }
  def sequence = rep1(value)
  def value = note | chord | ignored
  def note = rep(accidental) ~ v ~ rep(octaveModifier) ~ opt(duration) ^^ {
    case p ~ v ~ s ~ d ⇒ Value(p, v, s, d)
  }
  def chord = '[' ~> rep1(note) <~ ']' ^^ { case notes ⇒ Chord(notes) }
  def ignored = acceptMatch("ignored", {
    case c if " \n|()~" contains c ⇒ Ignored()
  })
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
  def duration = (opt(opt(digit) <~ accept('/')) ~ digit) ^^ {
    case None ~ n ⇒ Duration(n, 1)
    case Some(None) ~ d ⇒ Duration(1, d)
    case Some(Some(n)) ~ d ⇒ Duration(n, d)
  } | accept('/') ^^ { case _ ⇒ Duration(1, 2) }
  def digit = acceptMatch("digit", {
    case c if "0123456789" contains c ⇒ c.asDigit
  })

  override type Elem = Char
}
