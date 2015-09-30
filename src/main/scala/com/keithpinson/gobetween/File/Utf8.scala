package com.keithpinson.gobetween.File

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/29/2015
 */

import java.nio.{ByteBuffer,CharBuffer}
import java.nio.charset.{Charset,CharacterCodingException}

/**
 * All Strings should be encoded to UTF-8 before encryption and then decoded from
 * UTF-8 to a String (UTF-16) after deciphering.
 *
 * Life would be easier if all character encoding was UTF-8. However, Java, and by
 * extension Scala, strings are encoded in UTF-16 or a two-byte characters.  In 1995
 * that made good sense -- "all characters can be encoded in two bytes, right?" Most of
 * the time UTF-8 looks like 7-bit ASCII characters and rolls over to two-bytes, and
 * then three, and four as the size of Unicode value required.
 *
 * The problem with UTF-16 is that most of the time for text the second byte is unused
 * and when converting to a byte array, every other byte is zero. If the text is encrypted
 * before it is transmitted, then compression won't be very effective. If, on the
 * other hand, the string is converted to UTF-8, then all the "every other byte zeros" will
 * have disappeared and the resulting string be much smaller. (Technically, compression of
 * the string should happen before encoding also).
 *
 * So, this little class is to facilitate converting between Strings and UTF-8 and vice versa
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class Utf8 {

  private final val CHARSET = Charset.forName("UTF-8")


  def encodeToUTF8( plaintext:String ) : Array[Byte] = {
    try {
      val bytes: ByteBuffer = CHARSET.newEncoder.encode(CharBuffer.wrap(plaintext))
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
      CHARSET.newDecoder.decode(ByteBuffer.wrap(utf8Text)).toString
    }
    catch {
      case e: CharacterCodingException => throw e
    }
  }
}
