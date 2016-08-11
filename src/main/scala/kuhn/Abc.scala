package kuhn

import kuhn.ƒ._
import Math._

import scala.util.parsing.input.CharSequenceReader

object Abc extends scala.util.parsing.combinator.Parsers {

//  def parse[T <: Event[T]](in: String, scale: Scale): NoteOfScalePart[NoteOfScale] = {
//    val items = phrase(sequence)(new CharSequenceReader(in)) match {
//      case Success(items, _) ⇒ items
//      case n: NoSuccess ⇒ sys.error(n.toString)
//    }
//    var time = 0
//    var result = List.empty[NoteOfScale]
//    for (item ← items) yield {
//      item match {
//        case v: Note ⇒
//          if (!v.rest) {
//            result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = v.d, accidental = v.a)
//          }
//          time += v.duration.map(_.value).getOrElse(1*beats)
//        case Chord(notes) ⇒
//          notes foreach { v ⇒
//            if (!v.rest) {
//              result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = v.d, accidental = v.a)
//            }
//          }
//          time += notes.maxBy(_.d).d
//        case Tuplet(numberOfNotes, inTheTimeOf, _, notes) ⇒
//          notes foreach { v ⇒
//            val duration = v.d productWithRatio (inTheTimeOf, numberOfNotes)
//            if (!v.rest) {
//              result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = duration, accidental = v.a)
//            }
//            time += duration
//          }
//        case DottedCouplet(note1, note2, quantity) ⇒
//          var multiplier: (Int, Int) = (1, 2)
//          for (_ ← 1 until abs(quantity)) {
//            val (numerator, denominator) = multiplier
//            multiplier = ((numerator * 2) + 1, denominator * 2)
//          }
//          val d1 = note1.d + (note1.d productWithRatio multiplier) * (if (quantity > 0) 1 else -1)
//          val d2 = note2.d + (note2.d productWithRatio multiplier) * (if (quantity < 0) 1 else -1)
//          if (!note1.rest) {
//            result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(note1, scale), duration = d1, accidental = note1.a)
//          }
//          time += d1
//          if (!note2.rest) {
//            result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(note2, scale), duration = d2, accidental = note2.a)
//          }
//          time += d2
//      }
//    }
//    NoteOfScalePart(NoteOfScaleSequence(result, time))
//  }

  private def abcValueToNoteOfScaleRank(v: Note, scale: Scale): Int = {
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
  case class Note(accidentals: List[Accidental], value: Char, octaveModifiers: List[OctaveModifier], duration: Option[Duration]) extends Item {
    def a = accidentals.map(_.value).sum
    def rest = "ZXzx" contains value
    def v = value toUpper
    def o = octaveModifiers.map(_.value).sum + (if (value.isLower) 1 else 0)
    def d = duration.map(_.value).getOrElse(1*beats)
  }
  case class Chord(notes: List[Note]) extends Item
  case class Tuplet(numberOfNotes: Int, inTheTimeOf: Int, forTheNextNotes: Int, notes: List[Note]) extends Item
  case class DottedCouplet(note1: Note, note2: Note, quantity: Int) extends Item
  case class Accidental(value: Int)
  case class OctaveModifier(value: Int)
  case class Duration(numerator: Int, denominator: Int) {
    val value = beats / denominator * numerator
  }
  def sequence = rep1(item | ignored) ^^ { _ collect { case i: Item ⇒ i } }
  def item = dottedCouplet | note | chord | (tupletPrefix >> tuplet)
  def note = rep(accidental) ~ v ~ rep(octaveModifier) ~ opt(duration) ^^ {
    case p ~ v ~ s ~ d ⇒ Note(p, v, s, d)
  }
  def chord = '[' ~> rep1(note) <~ ']' ^^ { case notes ⇒ Chord(notes) }
  def ignored = acceptMatch("ignored", {
    case c if " \n\t|()~-" contains c ⇒ Unit
  })
  def accidental = acceptMatch("accidental", {
    case '^' ⇒ Accidental(+1)
    case '=' ⇒ Accidental(0)
    case '_' ⇒ Accidental(-1)
  })
  def v = acceptMatch("value", {
    case c if "CDEFGABcdefgabZXzx" contains c ⇒ c
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
  def tupletPrefix =
    '(' ~ digit ~ ':' ~ opt(digit) ~ ':' ~ opt(digit) ^^ {
      case '(' ~ numberOfNotes ~ ':' ~ inTheTimeOf ~ ':' ~ forTheNextNotes ⇒
        Tuplet(numberOfNotes, inTheTimeOf getOrElse tupletNumberOfNotesDefault(numberOfNotes), forTheNextNotes getOrElse numberOfNotes, null)
    } |
    '(' ~ digit ~ ':' ~ opt(digit) ^^ {
      case '(' ~ numberOfNotes ~ ':' ~ inTheTimeOf ⇒
        Tuplet(numberOfNotes, inTheTimeOf getOrElse tupletNumberOfNotesDefault(numberOfNotes), numberOfNotes, null)
    } |
    '(' ~ digit ^^ {
      case '(' ~ numberOfNotes ⇒
        Tuplet(numberOfNotes, tupletNumberOfNotesDefault(numberOfNotes), numberOfNotes, null)
    }
  private val tupletNumberOfNotesDefault: Int ⇒ Int = {
    case 2 ⇒ 3 // 2 notes in the time of 3
    case 3 ⇒ 2 // 3 notes in the time of 2
    case 4 ⇒ 3 // 4 notes in the time of 3
    case 5 ⇒ 2 // 5 notes in the time of n !!! (violated spec)
    case 6 ⇒ 2 // 6 notes in the time of 2
    case 7 ⇒ 2 // 7 notes in the time of n !!! (violated spec)
    case 8 ⇒ 3 // 8 notes in the time of 3
    case 9 ⇒ 2 // 9 notes in the time of n !!! (violated spec)
  }
  def tuplet(prefix: Tuplet) = repN(prefix.forTheNextNotes, note) ^^ {
    case notes ⇒ prefix.copy(notes = notes)
  }
  def dottedCouplet =
    note ~ rep1('<') ~ note ^^ {
      case note1 ~ mods ~ note2 ⇒ DottedCouplet(note1, note2, -mods.size)
    } |
    note ~ rep1('>') ~ note ^^ {
      case note1 ~ mods ~ note2 ⇒ DottedCouplet(note1, note2, +mods.size)
    }

  override type Elem = Char
}
