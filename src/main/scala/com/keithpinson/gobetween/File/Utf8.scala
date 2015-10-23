package com.keithpinson.gobetween.File

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/29/2015
 */

import java.nio.{ByteBuffer,CharBuffer}
import java.nio.charset.{CodingErrorAction, MalformedInputException, Charset, CharacterCodingException}
import java.lang.Integer.parseInt

/**
 * This little class is to facilitate converting between Java Strings and UTF-8, and
 * vice versa.
 *
 * Some History:
 *
 * UTF-8 can encode all Unicode code points. For ASCII characters its encoding is the
 * same and is entirely indistinguishable. For other characters a pattern of setting
 * the high bit is used to indicate that the character length is more than one byte.
 *
 * UTF-8 may be the best encoding we have for western hemisphere text. However it is not
 * without its flaws: multiple UTF-8 characters can decode to a single Unicode code point
 * and valid UTF-8 characters can decode to malformed UTF-16 characters.
 *
 * Life would be easier for a programmer if a string had only one encoding where the bit
 * value unambiguously identified the Unicode code point. In 1995 the thinking was,
 * "two bytes will encode all known characters" (Unicode had been that way for a decade.)
 * By 1996, Unicode ran out of space and needed more bits.
 *
 * Java and by extension, Scala, chose two-byte characters. It was an unfortunate mistake
 * that could not be un-done, so internally UTF-16 is used which means that in some cases
 * a character is more than two-bytes.
 *
 * UTF-8 Format:
 *
 * Most of the time UTF-8 looks like 7-bit ASCII characters and rolls over to two-bytes,
 * and then three, and four as the size of Unicode value required.
 *
 *    #   Code Points     Bits  Bit Pattern
 *   ---  --------------  ----  ------------------------------------------------
 *    1                    7    0xxxxxxx
 *        U+0000..U+007F        00..7F
 *
 *    2                    11   110xxxxx    10xxxxxx
 *        U+0080..U+07FF        C0..DF      80..BF
 *
 *    3                    16   1110xxxx    10xxxxxx    10xxxxxx
 *        U+0800..U+0FFF        E0          A0..BF      80..BF
 *        U+1000..U+FFFF        E1..EF      80..BF      80..BF
 *
 *    4                    21   11110xxx    10xxxxxx    10xxxxxx    10xxxxxx
 *        U+10000..U+3FFFF      F0          80..BF      80..BF      80..BF
 *        U+40000..U+FFFFF      F1..F3      80..BF      80..BF      80..BF
 *        U+100000..U10FFFF     F4          80..8F      80..BF      80..BF
 *
 * If we convert to a byte array we can see the difference between encodings. The following
 * string, "Holy Çow!" (where the C is \u2102, a cedilla) would be encoded in Java as:
 *
 *                                                                ---- U+2102 ---
 *   UTF-16 : Array(-2, -1, 0, 72, 0, 111, 0, 108, 0, 121, 0, 32, 33, 2,           0, 111, 0, 119, 0, 33)  (20 Bytes long)
 *   UTF-8  : Array(           72,    111,    108,    121,    32, -30, -124, -126,    111,    119,    33)  (11 Bytes Long)
 *   ASCII  : Array(           72,    111,    108,    121,    32, 63,                 111,    119,    33)  (9 Bytes, Ç becomes ?)
 *
 *                        If no encoding is specified, Java will replace non-ascii characters with a ? (ascii value 63)
 *
 * Tips for Use:
 *
 * In general, all Strings should be encoded to UTF-8 before encryption and then decoded
 * from UTF-8 to a Java String after deciphering; if the string is compressed before
 * encryption, then the encoding to UTF-8 can be skipped.
 *
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
case class Utf8( private var tempStr:String ) {

  private val utf8 = tempStr.getBytes("UTF-8")

  tempStr = "";tempStr=null

  override def toString = new String(utf8, "UTF-8")

  def getBytes = utf8
}
