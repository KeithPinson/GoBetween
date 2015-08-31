package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 8/27/2015
 */

import com.keithpinson.gobetween.PercentEncodedByte
import com.keithpinson.gobetween.PercentEncodedByte._
import org.specs2.specification.core.Fragments
import org.specs2.{Specification, ScalaCheck }
import org.scalacheck.{Arbitrary, Gen, Prop}

/**
 * Test encoding and decode of percent encoded bytes, eg. %20
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class PercentEncodedByteTest extends Specification with ScalaCheck { def is = skipAllIf(false) ^
  "There are two similar Url encoding schemes in use:" ^ br ^
  "UrlEncoding" ^ {new Url_EncodingTest} ^
  "Content type \"application/x-www-form-urlencoded\"" ^ {new Www_Form_Url_EncodingTest} ^
  "Both types of Percent Encoding need to result in a UTF-8 string" ! checkUTF8 ^
  "Percent encoding converts a Byte size character to its numeric value with a prepended \"%\", resulting in a 3 character string" ! checkPercent_Encode ^
  "The Percent Encoded url needs to have a corresponding decoding ability for:" ^ br ^
  "Decoding of UrlEncoding" ! checkDecodeURLEncoding ^
  "Decoding of content type \"application/x-www-form-urlencoded\"" ! checkDecodingWwwFormEncoding ^
  end

/*
// Need to get the code above to use the following s2 style but for now doing something wrong and the "new" is not becoming a fragment
def is = s2"""
  There are two similar Url encoding schemes in use:
    UrlEncoding ${new Url_EncodingTest}
    Content type "application/x-www-form-urlencoded" ${new Www_Form_Url_EncodingTest}
  Both types of Percent Encoding need to result in a UTF-8 string $checkUTF8
  Percent encoding converts a Byte size character to its numeric value with a prepended "%", resulting in a 3 character string $checkPercent_Encode
  The Percent Encoded url needs to have a corresponding decoding ability for:
  Decoding of UrlEncoding $checkDecodeURLEncoding
  Decoding of content type "application/x-www-form-urlencoded" $checkDecodingWwwFormEncoding
"""
*/

  def checkUTF8 = {
    val rawStr = "The string ü@foo-bar"
    val expectedStr = "The+string+%C3%BC%40foo-bar"

    encode( rawStr, isWwwForm=true ) mustEqual expectedStr
  }

  def checkPercent_Encode = {
    val rawStr = " "
    val expectedStr = "%20"

    encode( rawStr ) mustEqual expectedStr
  }

  val maxUrlLength = 2083

  def genString : Gen[String] = for { cc <- Gen.nonEmptyListOf(for {n <- Gen.chooseNum(0x0000,0x26FF)} yield n.toChar) } yield cc.take(maxUrlLength).mkString

  def checkDecodeURLEncoding = prop( (s:String) => PercentEncodedByte.decode(PercentEncodedByte.encode(s)) must beEqualTo(s) ).setGen(genString)

  def checkDecodingWwwFormEncoding = prop( (s:String) => PercentEncodedByte.decode(PercentEncodedByte.encode(s, isWwwForm = true), isWwwForm = true) must beEqualTo(s) ).setGen(genString)
}

class Url_EncodingTest extends Specification with ScalaCheck with UrlTestHelpers { def is = s2"""
  C0 Control codes, U+0000 to U+001F, inclusive, are Percent Encoded $checkC0_Controls
  Code points greater than U+007E are Percent Encoded $checkGreaterThanASCII
  Space (U+0020) is Percent Encoded $checkSpace
  Of the following special characters:
    ~!@#$$%^&*()_+`-={}|[]\:";'<>?,./
    ~!  $$  &*()_+ -          '   ,.     Are not Percent Encoded $checkNotEncoded
      @#  %^      ` ={}|[]\:"; <>?  /    Are Percent Encoded $checkEncoded
"""

  def genGreaterThanASCII : Gen[Char] = for { n <- Gen.chooseNum(0x007F,0x26FF) } yield n.toChar

  def checkC0_Controls = prop( (c0:Char) => PercentEncodedByte.encode(c0.toString) must beMatching("(\\%[0-9A-F]{2}){1}".r) ).setGen(genC0_Controls)
  def checkGreaterThanASCII = prop( (gta:Char) => PercentEncodedByte.encode(gta.toString) must beMatching("(\\%[0-9A-F]{2}){1,3}".r) ).setGen(genGreaterThanASCII)
  def checkSpace = PercentEncodedByte.encode(" ") must beEqualTo("%20")

  def checkNotEncoded = """ ~!  $  &*()_+ -          '   ,.""".foldLeft(Fragments.empty) {
    case (res,c) if c != ' ' =>
      res.append(c + " is not encoded" ! {PercentEncodedByte.encode(c.toString) must beEqualTo(c.toString)})

    case (res,c) =>
      res
  }

  def checkEncoded =    """  @# %^      ` ={}|[]\\:"; <>?  /""".foldLeft(Fragments.empty) {
    case (res,c) if c != ' ' =>
    res.append(c + " is encoded" ! {PercentEncodedByte.encode(c.toString) must beMatching("(\\%[0-9A-F]{2}){1}".r)})

    case (res,c) =>
      res
  }
}

class Www_Form_Url_EncodingTest extends Specification with ScalaCheck with UrlTestHelpers { def is = s2"""
  Space (U+0020) is converted into a plus sign (+) $checkSpace
  The alphanumeric characters "a" through "z", "A" through "Z" and "0" through "9" are not encoded $checkAlphanumeric
  The following special characters are not encoded:
    .-*_                                                         ${checkNotEncoded(".-*_")}
  Specifically, of the following special characters:
    ~!@#$$%^&*()_+`-={}|[]\:";'<>?,./
            *  _  -               .     Are not Percent Encoded ${checkNotEncoded("""         *  _  -               . """)}
    ~!@#$$%^& () +` ={}|[]\:";'<>?, /    Are Percent Encoded        ${checkEncoded("""~!@#$$%^& () +` ={}|[]\:";'<>?, /""")}
  All other characters are Percent Encoded $checkAllOthersEncoded
  """

  def checkSpace = PercentEncodedByte.encode(" ", isWwwForm = true) must beEqualTo("+")
  def checkAlphanumeric = prop( (a:Char) => PercentEncodedByte.encode(a.toString, isWwwForm = true) must beEqualTo(a.toString) ).setGen(genASCII_Alphanumeric)

  def checkNotEncoded( s:String ) = s.foldLeft(Fragments.empty) {
    case (res,c) if c != ' ' =>
      res.append(c + " is not encoded" ! {PercentEncodedByte.encode(c.toString, isWwwForm = true) must beEqualTo(c.toString)})

    case (res,c) =>
      res
  }

  def checkEncoded( s:String ) = s.foldLeft(Fragments.empty) {
    case (res,c) if c != ' ' =>
      res.append(c + " is encoded" ! {PercentEncodedByte.encode(c.toString, isWwwForm = true) must beMatching("(\\%[0-9A-F]{2}){1}".r)})

    case (res,c) =>
      res
  }

  def checkAllOthersEncoded = prop( (a:Char) => PercentEncodedByte.encode(a.toString) must beMatching("(\\%[0-9A-F]{2}){1,3}".r) ).setGen(genAllOthers)

  def genAllOthers : Gen[Char] = for { n <- Gen.chooseNum(0x0000,0x26FF) if n < 0x20 || n > 0x7E } yield n.toChar
}
