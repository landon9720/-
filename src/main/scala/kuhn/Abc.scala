package kuhn

import kuhn.ƒ._

import scala.util.parsing.input.CharSequenceReader

object Abc extends scala.util.parsing.combinator.Parsers {

  def parse[T <: Event[T]](in: String, scale: Scale): NoteOfScalePart[NoteOfScale] = {
    val items = sequence(new CharSequenceReader(in)).get
    println(s"items=$items")
    var time = 0
    var result = List.empty[NoteOfScale]
    for (item ← items) yield {
      item match {
        case v: Note ⇒
          if (!v.rest) {
            result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = v.d, accidental = v.a)
          }
          time += v.duration.map(_.value).getOrElse(1*beats)
        case Chord(notes) ⇒
          notes foreach { v ⇒
            if (!v.rest) {
              result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = v.d, accidental = v.a)
            }
          }
          time += notes.maxBy(_.d).d
        case Tuplet(numberOfNotes, inTheTimeOf, _, notes) ⇒
          notes foreach { v ⇒
            val duration = v.d productWithRatio (inTheTimeOf, numberOfNotes)
            if (!v.rest) {
              result :+= NoteOfScale(time = time, value = abcValueToNoteOfScaleRank(v, scale), duration = duration, accidental = v.a)
            }
            time += duration
          }
      }
    }
    println(s"result=$result time=$time")
    NoteOfScalePart(NoteOfScaleSequence(result, time))
  }

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
  case class Accidental(value: Int)
  case class OctaveModifier(value: Int)
  case class Duration(numerator: Int, denominator: Int) {
    val value = beats / denominator * numerator
  }
  def sequence = rep1(item | ignored) ^^ { _ collect { case i: Item ⇒ i } }
  def item = note | chord | (tupletPrefix >> tuplet)
  def note = rep(accidental) ~ v ~ rep(octaveModifier) ~ opt(duration) ^^ {
    case p ~ v ~ s ~ d ⇒ Note(p, v, s, d)
  }
  def chord = '[' ~> rep1(note) <~ ']' ^^ { case notes ⇒ Chord(notes) }
  def ignored = acceptMatch("ignored", {
    case c if " \n\t|()~" contains c ⇒ Unit
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
    case 2 ⇒ 3 // (2	2 notes in the time of 3
    case 3 ⇒ 2 // (3	3 notes in the time of 2
    case 4 ⇒ 3 // (4	4 notes in the time of 3
    case 5 ⇒ 2 // (5	5 notes in the time of n !!! (violated spec)
    case 6 ⇒ 2 // (6	6 notes in the time of 2
    case 7 ⇒ 2 // (7	7 notes in the time of n !!! (violated spec)
    case 8 ⇒ 3 // (8	8 notes in the time of 3
    case 9 ⇒ 2 // (9	9 notes in the time of n !!! (violated spec)
  }
  def tuplet(prefix: Tuplet) = repN(prefix.forTheNextNotes, note) ^^ {
    case notes ⇒ prefix.copy(notes = notes)
  }

  override type Elem = Char
}


//
//If the time signature is compound (6/8, 9/8, 12/8) then n is three, otherwise n is two.
//
//
//(p:q:r which means
//'put p notes into the time of q for the next r notes'.
//If q is not given, it defaults as above.
//If r is not given, it defaults to p.
//
//(3 is equivalent to (3:: or (3:2 ,
//which in turn are equivalent to (3:2:3,
//whereas (3::2 is equivalent to (3:2:2.
//
//This can be useful to include notes of different lengths within a tuplet,
//for example (3:2:2 G4c2 or (3:2:4 G2A2Bc.
//It also describes more precisely how the simple syntax works in cases like
//(3 D2E2F2 or even (3 D3EF2. The number written over the tuplet is p.

