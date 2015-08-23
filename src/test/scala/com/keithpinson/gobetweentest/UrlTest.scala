package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 8/22/2015
 */

import com.keithpinson.gobetween.Url._
import org.scalacheck.{Gen, Prop}
import org.specs2.{Specification, ScalaCheck }

/**
 * Test the Url class using ScalaCheck and Specs2 as specified in the URL Requirements.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class URLTest extends Specification { def is = sequential ^
  new TerminologyTest
}

trait UrlTestHelpers {

  def genC0_Controls : Gen[Char] = 'a'

  def genC0_Controls_and_Space : Gen[Char] = 'a'

  def genASCII_Digits : Gen[Char] = '1'

  def genASCII_Hex_Digits : Gen[Char] = '1'

  def genASCII_Alpha : Gen[Char] = 'a'

  def genASCII_Alphanumeric : Gen[Char] = 'a'

  def genASCII_String : Gen[String] = "a"

  def convert_to_ASCII_Lowercase( s:String ) = s.toLowerCase

  def serialize_an_Integer( i:Int ) = i.toString

  def genWindows_Drive_Letter : Gen[String] = "c:"

  def genNormalized_Windows_Drive_Letter : Gen[String] = "d:"
}

class TerminologyTest extends Specification with ScalaCheck with UrlTestHelpers { def is = s2"""
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

  def checkC0_Controls = prop( (c0:Char) => false)

  def checkC0_Controls_and_Space = prop( (c0:Char) => false)

  def checkASCII_Digits = prop( (d:Char) => false)

  def checkASCII_Hex_Digits = prop( (h:Char) => false)

  def checkASCII_Alpha = prop( (a:Char) => false)

  def checkASCII_Alphanumeric = prop( (a:Char) => false)

  def checkASCII_String = prop( (s:String) => false)

  def checkConvert_to_ASCII_Lowercase = prop( (s:String) => false)

  def checkSerialize_an_Integer = prop( (i:Int) => false)

  def checkWindows_Drive_Letter = prop( (s:String) => false)

  def checkNormalized_Windows_Drive_Letter = prop( (s:String) => false)
}

