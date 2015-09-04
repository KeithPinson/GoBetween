package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 8/22/2015
 */

import com.keithpinson.gobetween.Markdown._
import org.specs2.{Specification, ScalaCheck }
import org.scalacheck.{Arbitrary, Gen, Prop}


/**
 * Test the Markdown class using ScalaCheck and Specs2 as specified in the Markdown Requirements.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class MarkdownTest extends Specification { def is = sequential ^
  "Markdown's original purpose was to convert type written text to html markup" ^ br ^
  "For our purposes, Markdown's job (including the embedded html) is to convert text to the GoBetween Intermediate Structure" ^ br ^
  new MarkdownTerminologyTest
}

trait MarkdownTestHelpers {
  def genUnicode : Gen[Char] = for { c <- Gen.chooseNum(0x0000,0x26FF) } yield c.toChar
  def genLine : Gen[String] = for { cc <- Gen.nonEmptyListOf(Gen.alphaNumChar); e <- genEOL } yield cc.take(80).mkString + e
  def genEOL : Gen[String] = for { cc <- Gen.oneOf("\n","\r","\r\n") } yield cc
  def genBlankLine : Gen[String] = for { ww <- genWhitespace; cc <- genEOL } yield ww + cc
  def genWhitespace : Gen[String] = for { ww <- Gen.listOf(for {w <- Gen.oneOf(ws)} yield w) } yield ww.take(60).mkString

  private val ws = List(
    "0x0009", // horizontal tab
    "0x000B", // vertical tab
    "0x000C", // form feed
    "0x0020", // space
    "0x00A0", // no-break space
    "0x1680", // ogham space mark
    "0x2000", // en quad
    "0x2001", // em quad
    "0x2002", // en space
    "0x2003", // em space
    "0x2004", // three-per-em space
    "0x2005", // four-per-em space
    "0x2006", // six-per-em-space
    "0x2007", // figure space
    "0x2008", // punctuation space
    "0x2009", // thin space
    "0x200A", // hair space
    "0x202F", // narrow no-break space
    "0x205F", // medium mathematical space
    "0x3000" // ideographic space
  )

  //  def genSafeString : Gen[String] = for { cc <- Gen.nonEmptyListOf(for {n <- Gen.chooseNum(0x0000,0x26FF)} yield n.toChar) } yield cc.take(maxLength).mkString

}

class MarkdownTerminologyTest extends Specification with ScalaCheck with MarkdownTestHelpers { def is = s2"""
  The Markdown Requirements Document defines the following terms to mean:

  A character is a unicode code point $checkCharacter
  Any file encoding can be used, ie. "UTF-8" or "windows-1252", so the size of the code point depends on the file encoding being used

  A line is a sequence of zero or more characters followed by an end of line $checkLine

  An end of line is one of the following:
      An end of File
      A newline (U+000A)
      A carriage return (U+000D)
      Or, a carriage return + newline (U+000D,U+000A) $checkEOL

  A line containing no characters after all the whitespace has been removed is a blank line $checkBlankLine

"""

  def checkCharacter = prop( (c:Char) => c must beBetween(0x0000.toChar,0x26FF.toChar) ).setGen(genUnicode)

  def checkLine = prop( (s:String) => s must beMatching(""".*(\n|\r|\r\n)$""") ).setGen(genLine)

  def checkEOL = prop( (s:String) => s.lengthCompare(2) <= 0 && (s must beMatching("""(\n|\r|\r\n)$""")) ).setGen(genEOL)

  def checkBlankLine = prop( (s:String) => s must beMatching("""^[\p{Zs}]*(\n|\r|\r\n)$""") ).setGen(genBlankLine)
}

/*
    [[Blank Line]]
    A line containing no characters after all the whitespace has been removed is a blank line[[Whitespace]]

*/