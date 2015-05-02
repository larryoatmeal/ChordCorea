package models.music

import play.api.Logger
import models.utility._

case class MidiNote(midi: Integer)

case class DisplayNote(letter: NoteLetters.Value, sharpFlat: SharpFlats.Value)

case class DisplayNoteWithOctave(displayNote: DisplayNote, octave: Integer)

object NoteUtils{//parses notes into DisplayNote and DisplayNoteWithOctave

  //Parsing

  val letterMap = Map(
    "A" -> NoteLetters.A, 
    "B" -> NoteLetters.B, 
    "C" -> NoteLetters.C,
    "D" -> NoteLetters.D,
    "E" -> NoteLetters.E,
    "F" -> NoteLetters.F,
    "G" -> NoteLetters.G,
    "a" -> NoteLetters.A, 
    "b" -> NoteLetters.B, 
    "c" -> NoteLetters.C,
    "d" -> NoteLetters.D,
    "e" -> NoteLetters.E,
    "f" -> NoteLetters.F,
    "g" -> NoteLetters.G
  )

  val sharpFlatMap = Map(
    "##" -> SharpFlats.DoubleSharp,
    "bb" -> SharpFlats.DoubleFlat,
    "b" -> SharpFlats.Flat,
    "#" -> SharpFlats.Sharp,
    "N" -> SharpFlats.Natural,
    "n" -> SharpFlats.Natural
  )
  // n for natural
  val pattern = "^([ABCDEFGabcdefg])(##|bb|[b#Nn])?$".r

  def parse(str: String):Option[DisplayNote] = {
    str match {
      case pattern(letter, null) => {
        Some(DisplayNote(letterMap(letter), SharpFlats.Nothing))
      }
      case pattern(letter, suffix) => {
        Some(DisplayNote(letterMap(letter), sharpFlatMap(suffix)))
      }
      case _ => {
        Logger.error("INVALID INPUT")
        None
      }
    }
  }

  val patternOctave = "(.*?)(-?[0-9]{1,2})$".r //.*? for lazy, instead of greedy. Match any integer with two digits. Pos/neg

  def parseWithOctave(str: String):Option[DisplayNoteWithOctave] = {
    str match {
      case patternOctave(before, number) => {
        parse(before) match {
          case Some(displayNote) => {
            Some(DisplayNoteWithOctave(displayNote, number.toInt))//toInt guaranteed to work by pattern matching
          }
          case _ => {
            None
          }
        }
      }
      case _ => {
        None
      }
    }
  }

  //MIDI conversion

  def midify(displayNoteWithOctave: DisplayNoteWithOctave) = {
    val base = displayNoteWithOctave.displayNote.letter match {
      case NoteLetters.C => 0
      case NoteLetters.D => 2
      case NoteLetters.E => 4
      case NoteLetters.F => 5
      case NoteLetters.G => 7
      case NoteLetters.A => 9
      case NoteLetters.B => 11 
      case _ => {
        Logger.error("midify: Invalid NoteLetters")
        0
      }
    }

    val offset = displayNoteWithOctave.displayNote.sharpFlat match {
      case SharpFlats.Flat => -1
      case SharpFlats.Sharp => 1
      case SharpFlats.DoubleFlat => -2
      case SharpFlats.DoubleSharp => 2
      case SharpFlats.Nothing | SharpFlats.Natural => 0
      case _ => {
        Logger.error("midify: Invalid sharpFlat")
        0
      }
    }

    MidiNote(base + offset + octaveOffset(displayNoteWithOctave.octave))
  }

  def octaveOffset(octave: Integer) = {
    (octave + 1) * 12 //C4 is 60
  }

}



