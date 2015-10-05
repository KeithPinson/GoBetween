package com.keithpinson.gobetween.File

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/29/2015
 */

import java.nio.{ByteBuffer,CharBuffer}
import java.nio.charset.{CodingErrorAction, MalformedInputException, Charset, CharacterCodingException}

/**
 * All Strings should be encoded to UTF-8 before encryption and then decoded from
 * UTF-8 to a Java String after deciphering.
 *
 * Life would be easier if all character encoding was UTF-8. However, Java, and by
 * extension Scala, strings are encoded two-byte characters. Which, in 1995, made
 * good sense -- "all characters can be encoded in two bytes, right?" Well, no.  By
 * 1996, Unicode ran out of space and needed more bits. Choosing two-byte characters
 * without a clearly defined encoding, in hindsight, was a mistake. Most of the time
 * UTF-8 looks like 7-bit ASCII characters and rolls over to two-bytes, and
 * then three, and four as the size of Unicode value required.
 *
 *  #   Code Points     Bits  Bit Pattern
 * ---  --------------  ----  ------------------------------------------------
 *  1                    7    0xxxxxxx
 *      U+0000..U+007F        00..7F
 *
 *  2                    11   110xxxxx    10xxxxxx
 *      U+0080..U+07FF        C2..DF      80..BF
 *
 *  3                    16   1110xxxx    10xxxxxx    10xxxxxx
 *      U+0800..U+0FFF        E0          A0..BF      80..BF
 *      U+1000..U+FFFF        E1..EF      80..BF      80..BF
 *
 *  4                    21   11110xxx    10xxxxxx    10xxxxxx    10xxxxxx
 *      U+10000..U+3FFFF      F0          80..BF      80..BF      80..BF
 *      U+40000..U+FFFFF      F1..F3      80..BF      80..BF      80..BF
 *      U+100000..U10FFFF     F4          80..8F      80..BF      80..BF
 *
 * The problem with UTF-16 and other two-byte encodings is that most of the time for most
 * text the second byte is unused and when converting to a byte array, every other byte is
 * zero. If the text is encrypted before it is transmitted, then compression won't be very
 * effective. If, on the other hand, the string is converted to UTF-8, then all the "every
 * other byte zeros" will have disappeared and the resulting string will be much smaller.
 * (Technically, compression of the string should happen before encoding and depending on
 * the compression technique the extra bytes will not add much to the final size).
 *
 * So, this little class is to facilitate converting between Strings and UTF-8 and vice versa
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class Utf8 {

  private final val charset = Charset.forName("UTF-8")


  def encodeToUTF8( plaintext:String ) : Array[Byte] = {
    try {
      val bytes: ByteBuffer = charset.newEncoder.encode(CharBuffer.wrap(plaintext))
      val bytesCopy: Array[Byte] = new Array[Byte](bytes.limit)
      System.arraycopy(bytes.array, 0, bytesCopy, 0, bytes.limit)
      bytesCopy
    }
    catch {
      case e:CharacterCodingException => throw e
    }
  }

  def decodeFromUTF8( utf8Text:Array[Byte] ) : String = {
    try {
      val decoder = charset.newDecoder()
      decoder.onMalformedInput(CodingErrorAction.REPORT)
      decoder.decode(ByteBuffer.wrap(utf8Text)).toString
    }
    catch {
      case e:CharacterCodingException => throw e
      case e:java.nio.charset.MalformedInputException => throw e
    }
  }

  def isMalformed( utf8Text:Array[Byte] ) : Boolean = {

    /* Well-formed UTF-8

       0xxxxxxx
       110xxxxx    10xxxxxx
       1110xxxx    10xxxxxx    10xxxxxx
       11110xxx    10xxxxxx    10xxxxxx    10xxxxxx
     */
    def consume( u8:Array[Byte] ) : Array[Byte] = {

      if( u8.isEmpty ) u8
      else {
        val next = u8 match {
          case Array(a)                   if (a >>> 7) == 0 => Array[Byte]()
          case Array( a, b @ _*)          if (a >>> 7) == 0 => b.to[Array[Byte]]
          case Array( a, b, c @ _*)       if (a >>> 5) == 0x110 && (b >>> 6) == 0x10 => c.to[Array[Byte]]
          case Array( a, b, c, d @ _*)    if (a >>> 4) == 0x1110 && (b >>> 6) == 0x10 && (c >>> 6) == 0x10 => d.to[Array[Byte]]
          case Array( a, b, c, d, e @ _*) if (a >>> 3) == 0x11110 && (b >>> 6) == 0x10 && (c >>> 6) == 0x10 && (d >>> 6) == 0x10 => e.to[Array[Byte]]
          case b => return b // Short circuit
        }

        consume(next)
      }
    }

    consume( utf8Text ).nonEmpty
  }
}
