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

  val letterToBaseMidi = Map(
     NoteLetters.C -> 0,
     NoteLetters.D -> 2,
     NoteLetters.E -> 4,
     NoteLetters.F -> 5,
     NoteLetters.G -> 7,
     NoteLetters.A -> 9,
     NoteLetters.B -> 11
    )



  val sharpFlatToOffset = Map(
     SharpFlats.Flat -> -1,
     SharpFlats.Sharp -> 1,
     SharpFlats.DoubleFlat -> -2,
     SharpFlats.DoubleSharp -> 2,
     SharpFlats.Nothing -> 0,
     SharpFlats.Natural -> 0
    )

  val offsetToSharpFlat = sharpFlatToOffset.map(_.swap)


  def midify(displayNoteWithOctave: DisplayNoteWithOctave) = {
    val base = letterToBaseMidi(displayNoteWithOctave.displayNote.letter) 
    val offset = sharpFlatToOffset(displayNoteWithOctave.displayNote.sharpFlat)
    MidiNote(base + offset + octaveOffset(displayNoteWithOctave.octave))
  }

  def octaveOffset(octave: Integer) = {
    (octave + 1) * 12 //C4 is 60
  }


  // def closestOctaveToNote(midiNote: MidiNote):Integer= {
  //   val floored = (midiNote.midi / 12) * 12
  //   val ceiled = (midiNote.midi / 12 + 1) * 12 

  //   if(math.abs(floored - midiNote.midi) < math.abs(ceiled - midiNote.midi)){
  //     floored
  //   }
  //   else{
  //     ceiled
  //   }

  // }


  def closestNeighborMod(src: Integer, dst: Integer, n: Integer):Integer = {
    val m = MyMath.mod(dst-src,n)
    if(m > n/2){
      m - n
    }
    else{
      m
    }
  }


  def noteFromMidi(midiNote: MidiNote, letter: NoteLetters.Value) = {//inverse of midify
    val base = letterToBaseMidi(letter)
    

    Logger.debug(s"Base:$base, x:${midiNote.midi % 12}")

    val offset = closestNeighborMod(base, midiNote.midi % 12, 12)

    if(offsetToSharpFlat.contains(offset)){
      val sharpFlat = offsetToSharpFlat(offset)
      val octaves = midiNote.midi / 12 - 1
      DisplayNoteWithOctave(DisplayNote(letter, sharpFlat),octaves)
    }
    else{
      DisplayNoteWithOctave(DisplayNote(NoteLetters.C, SharpFlats.Nothing),-1)
      Logger.error(s"INVALID NOTE FROM MIDI:$midiNote,$letter")
    }

  }

  def testNoteToMidi(){
    val tests = List((60, NoteLetters.C),(61,NoteLetters.C),(62, NoteLetters.C),(60, NoteLetters.B),(55, NoteLetters.G))

    tests.foreach{
      case (midi, letter) =>{
        val note = noteFromMidi(MidiNote(midi),letter)
        Logger.debug(note.toString)
      }
    }
  }


}



