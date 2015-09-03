package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 8/22/2015
 */

import com.keithpinson.gobetween.Url._
import org.specs2.{Specification, ScalaCheck }
import org.scalacheck.{Arbitrary, Gen, Prop}

/**
 * Test the Url class using ScalaCheck and Specs2 as specified in the URL Requirements.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class URLTest extends Specification { def is = sequential ^
  new UrlTerminologyTest
}

trait UrlTestHelpers {

  val maxUrlLength = 2083

  /** C0 controls are code points in the range U+0000 to U+001F */
  def genC0_Controls : Gen[Char] = for { n <- Gen.chooseNum(0,0x001F) } yield n.toChar

  /** C0 controls and space are C0 controls and code point U+0020 */
  def genC0_Controls_and_Space : Gen[Char] = for { n <- Gen.chooseNum(0,0x0020) } yield n.toChar

  /** ASCII digits are code points in the range U+0030 to U+0039, inclusive. */
  def genASCII_Digits : Gen[Char] = Gen.numChar //for { n <- Gen.chooseNum(0x0030,0x0039)} yield n.toChar

  /** The ASCII hex digits are ASCII digits, code points in the range U+0041 to U+0046, inclusive, and code points in the range U+0061 to U+0066, inclusive. */
  def genASCII_Hex_Digits : Gen[Char] = nextHex

  /** The ASCII alpha are code points in the range U+0041 to U+005A, inclusive, and in the range U+0061 to U+007A, inclusive. */
  def genASCII_Alpha : Gen[Char] = Gen.alphaChar

  /** The ASCII alphanumeric are ASCII digits and ASCII alpha. */
  def genASCII_Alphanumeric : Gen[Char] = Gen.alphaNumChar

  /** An ASCII string is a string in the range U+0000 to U+007F, inclusive. */
  def genASCII_String : Gen[String] = for { cc <- Gen.nonEmptyListOf(for {n <- Gen.chooseNum(0x0000,0x007F)} yield n.toChar) } yield cc.take(maxUrlLength).mkString

  /** In a string convert ASCII upper case letters to lowercase by replacing all code points in the range U+0041 to U+005A, inclusive, with the corresponding code points in the range U+0061 to U+007A, inclusive. */
  def convert_to_ASCII_Lowercase( s:String ) = s.toLowerCase

  /** Shorten an integer to its shortest possible decimal number string */
  def serialize_an_Integer( i:Int ) = i.toString

  /** A Windows drive letter is two code points, of which the first is an ASCII alpha and the second is either ":" or "|". */
//  def genWindows_Drive_Letter : Gen[String] = Gen.alphaChar.toString.take(1) + Gen.oneOf(":","|")
  def genWindows_Drive_Letter : Gen[String] = for( a <- Gen.alphaChar; d <- Gen.oneOf(":","|") ) yield a.toString + d

  /** A normalized Windows drive letter is a Windows drive letter of which the second code point is ":". */
  def genNormalized_Windows_Drive_Letter : Gen[String] = for( a <- Gen.alphaChar ) yield a.toString + ":"

  /**
   * Get the next random hex with an equal chance that it will any of 16 digits and
   * an equal chance that the alpha digits will be upper- or lower-case.
   */
  def nextHex = {
    // Ignore lower-case here to keep the alpha characters from doubling their odds of being chosen
    def isHex(c: Char) = (c >= 'A' && c <= 'F') || (c >= '0' && c <= '9')

    def shouldBeUpper = scala.util.Random.nextBoolean()
    def toCase(c: Char, shouldBeUpper: Boolean) = c match {
      case x if !x.isLetter => x
      case x if shouldBeUpper => x.toUpper
      case x => x.toLower
    }

    val hh = (Iterator continually scala.util.Random.nextPrintableChar filter isHex).map(toCase(_, shouldBeUpper))

    hh.next()
  }

}

class UrlTerminologyTest extends Specification with ScalaCheck with UrlTestHelpers { def is = s2"""
  The URL Requirements Document defines the following terms to mean:

  C0 controls are code points in the range U+0000 to U+001F, inclusive $checkC0_Controls

  C0 controls and space are C0 controls and code point U+0020. $checkC0_Controls_and_Space

  ASCII digits are code points in the range U+0030 to U+0039, inclusive. $checkASCII_Digits

  The ASCII hex digits are ASCII digits, code points in the range U+0041 to U+0046, inclusive, and code points in the range U+0061 to U+0066, inclusive. $checkASCII_Hex_Digits

  The ASCII alpha are code points in the range U+0041 to U+005A, inclusive, and in the range U+0061 to U+007A, inclusive. $checkASCII_Alpha

  The ASCII alphanumeric are ASCII digits and ASCII alpha. $checkASCII_Alphanumeric

  An ASCII string is a string in the range U+0000 to U+007F, inclusive. $checkASCII_String

  In a string convert ASCII upper case letters to lowercase by replacing all code points in the range U+0041 to U+005A, inclusive, with the corresponding code points in the range U+0061 to U+007A, inclusive. $checkConvert_to_ASCII_Lowercase

  Shorten an integer to a decimal number without leading zeros $checkSerialize_an_Integer

  A Windows drive letter is two code points, of which the first is an ASCII alpha and the second is either ":" or "|". $checkWindows_Drive_Letter

  A normalized Windows drive letter is a Windows drive letter of which the second code point is ":". $checkNormalized_Windows_Drive_Letter
"""

  def checkC0_Controls = prop( (c0:Char) => c0 must beBetween(0x0000.toChar,0x001f.toChar) ).setGen(genC0_Controls)

  def checkC0_Controls_and_Space = prop( (c0s:Char) => c0s must beBetween(0x0000.toChar,0x0020.toChar) ).setGen(genC0_Controls_and_Space)

  def checkASCII_Digits = prop( (d:Char) => d must beBetween(0x0030.toChar,0x0039.toChar)).setGen(genASCII_Digits)

  def checkASCII_Hex_Digits = prop( (h:Char) => h must beBetween(0x0030.toChar,0x0039.toChar) or beBetween(0x0041.toChar,0x0046.toChar) or beBetween(0x0061.toChar,0x0066.toChar)).setGen(genASCII_Hex_Digits)

  def checkASCII_Alpha = prop( (a:Char) => a must beBetween(0x0041.toChar,0x005A.toChar) or beBetween(0x0061.toChar,0x007A.toChar)).setGen(genASCII_Alpha)

  def checkASCII_Alphanumeric = prop( (a:Char) => a must beBetween(0x0030.toChar,0x0039.toChar) or beBetween(0x0041.toChar,0x005A.toChar) or beBetween(0x0061.toChar,0x007A.toChar)).setGen(genASCII_Alphanumeric)

  def checkASCII_String = prop( (s:String) => s.forall( a => a must beBetween(0x0000.toChar,0x007F.toChar))).setGen(genASCII_String)

  def checkConvert_to_ASCII_Lowercase = convert_to_ASCII_Lowercase("ABCDEFGHIJKLMNOPQRSTUVWXYZ") mustEqual "abcdefghijklmnopqrstuvwxyz"

  def checkSerialize_an_Integer = serialize_an_Integer(87) mustEqual "87"

  def checkWindows_Drive_Letter = prop( (s:String) => s.lengthCompare(2) == 0 &&
    ((s.head >=  0x0041.toChar && s.head <= 0x005A.toChar) ||
      (s.head >= 0x0061.toChar && s.head <= 0x007A.toChar)) &&
    (s.last == ':' || s.last == '|')).setGen(genWindows_Drive_Letter)

  def checkNormalized_Windows_Drive_Letter = prop( (s:String) => s.lengthCompare(2) == 0 &&
    ((s.head >=  0x0041.toChar && s.head <= 0x005A.toChar) ||
      (s.head >= 0x0061.toChar && s.head <= 0x007A.toChar)) &&
    s.last == ':').setGen(genNormalized_Windows_Drive_Letter)
}

