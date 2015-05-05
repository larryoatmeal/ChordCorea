package models.music

import models.utility._
import play.api.Logger


case class Interval(intervalClass: Integer, quality: IntervalQualities.Value, difference:Integer){

  val ascending = difference > 0


  def simplify(maxClass: Integer = 13):(Interval, Integer) = {
    if(intervalClass <= maxClass){
      (this, 0)
    }
    else{
      val simplifiedIntervalClass = intervalClass % 7
      val extraOctaves = intervalClass / 7
      val interval = Interval(simplifiedIntervalClass, quality, difference)
      (interval, extraOctaves)
    }
  }

  def display(displayDirection: Boolean = true, maxClass: Integer = 13, displayExtraOctaves: Boolean = false):String = {
    
    val (interval, extraOctaves) = simplify(maxClass)
    val beforeDirection = MyString.optionalAppend(s"${interval.quality}${interval.intervalClass}", 
      s"+extraOctaves", displayExtraOctaves && extraOctaves > 0)

    MyString.optionalAppend(beforeDirection, if(ascending) " up" else " dn", displayDirection)
  }


  def transpose(note: DisplayNoteWithOctave) = {
    
    

  }


}


object Interval{


  val numberOfLetters = 7
  val numberOfNotes = 12
  val letterNumberMap = Map(
    NoteLetters.A -> 0,
    NoteLetters.B -> 1,
    NoteLetters.C -> 2,
    NoteLetters.D -> 3,
    NoteLetters.E -> 4,
    NoteLetters.F -> 5,
    NoteLetters.G -> 6
    )

  val numberLetterMap = letterNumberMap.map(_.swap)


  def test{
    val notes = List("A4","C4","G3","Ab4","Bb3","D4","D#4","E7","Ebb7","E##7","Cbb4","Cbb4").map(NoteUtils.parseWithOctave)
    val notePairs = notes.iterator.sliding(2).toList.map(_.flatten)
    val intervals = notePairs.foreach{
      notes => {
        Logger.debug(calculateInterval(notes(0), notes(1)).display())
      }
    }
  }

  val defaultStarting = Map(
    1 -> 0,
    2 -> 2,
    3 -> 4,
    4 -> 5,
    5 -> 7,
    6 -> 9,
    7 -> 11
    )

  val offsetForPerfects = Map(
    0 -> IntervalQualities.P,
    1 -> IntervalQualities.A,
    2 -> IntervalQualities.AA,
    3 -> IntervalQualities.AAA,
    4 -> IntervalQualities.AAAA,
    -1 -> IntervalQualities.d,
    -2 -> IntervalQualities.dd,
    -3 -> IntervalQualities.ddd,
    -4 -> IntervalQualities.dddd
    )

  val perfectsToOffset = offsetForPerfects.map(_.swap)


  val offsetForRegulars = Map(
    0 -> IntervalQualities.M,
    1 -> IntervalQualities.A,
    2 -> IntervalQualities.AA,
    3 -> IntervalQualities.AAA,
    4 -> IntervalQualities.AAAA,
    -1 -> IntervalQualities.m,
    -2 -> IntervalQualities.d,
    -3 -> IntervalQualities.dd,
    -4 -> IntervalQualities.ddd,
    -5 -> IntervalQualities.dddd
    )

  val regularsToOffset = offsetForRegulars.map(_.swap)



  def calculateInterval(noteA: DisplayNoteWithOctave, noteB:DisplayNoteWithOctave):Interval = {

    val difference = NoteUtils.midify(noteB).midi - NoteUtils.midify(noteA).midi

    val intervalSize = math.abs(difference % numberOfNotes) // -13 goes to 1 instead of -13 going to 11 in real mod
    val extraOctaves = math.abs(difference / numberOfNotes)

    val letterNumberA = letterNumberMap(noteA.displayNote.letter)
    val letterNumberB = letterNumberMap(noteB.displayNote.letter)

    val intervalClass = 
      (if (difference > 0) MyMath.mod(letterNumberB - letterNumberA, numberOfLetters) 
      else MyMath.mod(letterNumberA - letterNumberB, numberOfLetters))+1

    // Now we must figure out which quality interval is. 1, 4, 5 default perfect, 2,3,6,7

    val intervalOffsetFromDefault = intervalSize - defaultStarting(intervalClass) 





    val intervalQuality = intervalClass match{
      case 1 | 4 | 5 => 
        offsetForPerfects(intervalOffsetFromDefault)
      case 2 | 3 | 6 | 7 =>
        offsetForRegulars(intervalOffsetFromDefault)
    }


    val intervalClassOctaveExtend = intervalClass + numberOfLetters*extraOctaves
    Interval(intervalClassOctaveExtend, intervalQuality, difference)
  }



}