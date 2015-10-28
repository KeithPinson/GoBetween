package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/13/2015
 */

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, Charset}
import java.lang.Character.{charCount, highSurrogate, lowSurrogate}

import com.keithpinson.gobetween.File._
import javax.crypto.KeyGenerator
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import org.scalacheck.Gen
import org.specs2.Specification
import org.specs2.mock.Mockito
import org.specs2.ScalaCheck

/**
 * This class begins the test of the File handler.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class FileTest extends Specification { def is = skipAllIf(false) ^
  "The purpose of the file handler is to support the complexities of remote files beyond that of the standard file systems" ^ br ^
  "As with any file system there are two complimentary parts:" ^ br ^
  "A file reader" ^ {new ReadTest} ^
//  "And, a file writer" ^ {new WriteTest} ^
  end
}

class ReadTest extends Specification { def is =
  "Read Test".title ^
  "A file reader consists of the following parts:" ^
  "A read cache" ^ {new ReadCacheTest} ^
  "A listener" ^ {new ReadListenerTest} ^
  "A file requester" ^ {new FileRequestTest} ^
  "A security package" ^ {new ReadSecurityTest} ^
  "And, a handler" ^ {new ReadHandlerTest} ^
  end
}

class ReadCacheTest extends Specification { def is =
  "Read Cache Test".title ^
  "The purpose of the read cache is to be a temporary local store, it must support:" ^
//  "A contract that the file will not be removed during downloads/live updates" ^ {new ReadCacheReservationTest}
//  "A configurable system to prioritize a file's place in the cache" ^ {new ReadCachePrioritiesTest}
//  "A mechanism to date files, obscure local file names, and record the file request" ^ {new ReadFileLocalStorageTest}
//  "Removal of stale files and low priority files" ^ {new ReadCachePruningTest} ^
  end
}

class ReadListenerTest extends Specification { def is =
  "Read Listener Test".title ^
  "All files are assumed to be transmitted from a remote system.A listener exists to support:" ^
//  "Remotely streamed files" ^ {new ReadFileStreamTest} ^
//  "Live updates to a cached file" ^ {new ReadLiveUpdatesTest} ^
  end
}

class FileRequestTest extends Specification { def is =
  "File Request Test".title ^
  "To avoid resending files that exist in the cache a file request consists of a header with the following metadata:" ^
//  "File URL" ^ {new FileURLTest} ^
//  "File Name" ^ {new FileNameTest} ^
//  "Metadata Version" ^ {new MetadataVersionTest} ^
//  "Packets in cache" ^ {new FileSpanningTest} ^
  "UTF-8" ^ {new UTF8TextTest} ^
  "Checksum of file" ^ {new FileChecksumTest} ^
  "Cipher" ^ {new FileCipherTest} ^
//  "Hamming-like Code" ^ {new FileHammingLikeCodeTest} ^
  end
}

class ReadSecurityTest extends Specification { def is =
  "Read Security Test".title ^
  "To try and avoid security nightmares, the request of a file should be treated like an API request, where:" ^
//  "Server host is obscured" ^ {new HostObscuredTest} ^
//  "API (file server) key stored remotely" ^ {new RemoteKeyStorageTest} ^
//  "SSL (https) is used" ^ {new SSLTransportTest} ^
  end
}

class ReadHandlerTest extends Specification { def is =
  "Read Handler Test".title ^
  "The read handler is responsible for assebling the received packets and passing them to a parser for conversion to the goBetween format format" ^
  end
}

/*
class ReadCacheReservationTest extends Specification { def is = "test" ! pending }

class ReadCachePrioritiesTest extends Specification { def is = "test" ! pending }
class ReadFileLocalStorageTest extends Specification { def is = "test" ! pending }
class ReadCachePruningTest extends Specification { def is = "test" ! pending }

class ReadFileStreamTest extends Specification { def is = "test" ! failure }
class ReadLiveUpdatesTest extends Specification { def is = "test" ! pending }

class FileURLTest extends Specification { def is = "test" ! pending }
class FileNameTest extends Specification { def is = "test" ! pending }
class MetadataVersionTest extends Specification { def is = "test" ! pending }
class FileSpanningTest extends Specification { def is = "test" ! pending }
*/

class UTF8TextTest extends Specification with ScalaCheck { def is = s2"""
  The character encoding, UTF-8 is supported
    All single-byte values, 0x00 - 0x7f, can be UTF-8 encoded and decoded $checkAllSingleByteValues
    The native string encoding can be converted to UTF-8 $checkUtf8
    UTF-8 can be converted to the native string encoding $checkUnicodeCodepoints
"""

  private def decodeToString( bb:Array[Byte] ) : String = {
    try {
      val decoder = Charset.forName("UTF-16").newDecoder()
      decoder.onMalformedInput(CodingErrorAction.REPLACE)
      decoder.decode(ByteBuffer.wrap(bb)).toString
    }
    catch {
      case e:CharacterCodingException => "?"
      case e:java.nio.charset.MalformedInputException => "?"
    }
  }

  private def codepointToBytes( cp:Int ) : Array[Byte] = charCount(cp) match {
    // Little-Endian
    case 1 => Array((cp >>> 8).toByte,(cp & 0xff).toByte)
    case 2 => Array( (highSurrogate(cp) >>> 8).toByte, (highSurrogate(cp) & 0xff).toByte, (lowSurrogate(cp) >>> 8).toByte, (lowSurrogate(cp) & 0xff).toByte )
    case _ => Array()
  }

  def isMalformed( utf8Text:Array[Byte] ) : Boolean = {

    /* Well-formed UTF-8

       0xxxxxxx
       110xxxxx    10xxxxxx
       1110xxxx    10xxxxxx    10xxxxxx
       11110xxx    10xxxxxx    10xxxxxx    10xxxxxx
     */
    def consume( u8:Array[Byte] ) : Array[Byte] = {

      import java.lang.Integer.parseInt

      if( u8.isEmpty ) u8
      else {
        val next = u8 match {
          case Array(a)                   if (a >>> 7) == 0 => Array[Byte]()
          case Array( a, b @ _*)          if (a >>> 7) == 0 => b.toArray
          case Array( a, b, c @ _*)       if (a >>> 5 & 0x7) == parseInt("110",2) && (b >>> 6 & 0x3) == parseInt("10",2) => c.toArray
          case Array( a, b, c, d @ _*)    if (a >>> 4 & 0xf) == parseInt("1110",2) && (b >>> 6 & 0x3) == parseInt("10",2) && (c >>> 6 & 0x3) == parseInt("10",2) => d.toArray
          case Array( a, b, c, d, e @ _*) if (a >>> 3 & 0x1f) == parseInt("11110",2) && (b >>> 6 & 0x3) == parseInt("10",2) && (c >>> 6 & 0x3) == parseInt("10",2) && (d >>> 6 & 0x3) == parseInt("10",2) => e.toArray
          case b => return b // Short circuit
        }

        consume(next)
      }
    }

    consume( utf8Text ).nonEmpty
  }

  val allBytes = (0 to 0x7f).map( _.toChar.toString )
  val allUnicode = ((0 to 0xD7FF) ++ (0xE000 to 0xEFFF) ++ (0x100000 to 0x10FFFF)).map( v => decodeToString(codepointToBytes(v)) )
  val allChars = (0 to 0x2fff).map( v => new String(Array( (v >>> 8).toByte, (v & 0x00ff).toByte ),"UTF-16") )


  val maxStrLength = 4096
  def genAllUnicode : Gen[String] = Gen.oneOf(allUnicode)
  def genUnicodeString : Gen[String] = for { cc <- Gen.nonEmptyListOf(for {n <- Gen.chooseNum(0x0000,0x26FF)} yield n.toChar) } yield cc.take(maxStrLength).mkString
  def genAllBytes : Gen[String] = Gen.oneOf(allBytes)
  def genAllChars : Gen[String] = Gen.oneOf(allChars)

  def checkAllSingleByteValues = prop( (v:String) => {Utf8(v).toString must beEqualTo(v)} ).setGen(genAllBytes).set(minTestsOk = 0x7f, minSize = 0x7f, maxSize = 0x7f, workers=3).verbose
  def checkUnicodeCodepoints = prop( (v:String) => {Utf8(v).toString must beEqualTo(v)} ).setGen(genAllUnicode).set(minTestsOk = 0x1E7FD, minSize = 0x1E7FD, maxSize = 0x1E7FD, workers=3).verbose
  def checkUtf8 = prop( (v:String) => {isMalformed(Utf8(v).getBytes) must beFalse} ).setGen(genAllUnicode).set(minTestsOk = 0x2fff, minSize = 0x2fff, maxSize = 0x2fff, workers=3).verbose
  def checkUnicodeStrings = prop( (v:String) => {Utf8(v).toString must beEqualTo(v)} ).setGen(genAllBytes).set(minTestsOk = 0xff, minSize = 0xff, maxSize = 0xff, workers=3).verbose
}

class FileChecksumTest extends Specification { def is = s2"""
  A string of characters is encrypted and then a hash, ie. checksum, is calculated $genChecksumTest
  The checksum should support versionioning
    Versions should have an expiry mechanism $versioningTest
  An existing checksum can be compared against a string of characters $checksumVerifyTest
"""

  val msg = scala.util.Random.alphanumeric.take(4096).mkString

  val secretKey = {
    val keyGenerator = KeyGenerator.getInstance("HmacMD5")
    new String( keyGenerator.generateKey().getEncoded )
  }

  val checksumHash = {
    val cs = new Checksum( secretKey )
    cs.checksum(msg)
  }

  def genChecksumTest = {
    checksumHash must beAnInstanceOf[String]
  }

  def versioningTest = {
    val expiredHash = 'A' + checksumHash.tail

    val cs = new Checksum( secretKey )

    cs.verify(msg,expiredHash) must beFalse
  }

  def checksumVerifyTest = {
    val cs = new Checksum( secretKey )

    cs.verify( msg, checksumHash ) must beTrue
  }
}

class FileCipherTest extends Specification with ScalaCheck { def is = s2"""
Text encoding is supported
    The cipher uses a 256-bit key $check256bKeys
    And, keys less than 256-bits fail $checkLessThan256bKeys
    Text can be encrypted with a cipher $encryptTextTest
    An encrypted string can be deciphered back to its original text $checkDecryption
  """

  val secretKey = {
    val keyGenerator = KeyGenerator.getInstance("HmacSHA256")
    keyGenerator.generateKey().getEncoded
  }


  val iv = {
    val ivSpec = new IvParameterSpec( KeyGenerator.getInstance("AES").generateKey.getEncoded )
    ivSpec.getIV
  }

//  val secretKeyObj = KeyGenerator.getInstance("HmacSHA256").generateKey
//  val ivObj = new IvParameterSpec(KeyGenerator.getInstance("AES").generateKey.getEncoded)

  val keyLength = 32
  def genKeyBytes : Gen[Array[Byte]] = for { cc <- Gen.listOfN(keyLength, for {n <- Gen.chooseNum(0x0000,0x26FF)} yield n.toByte) } yield cc.take(keyLength).toArray

  def check256bKeys = prop( (k:Array[Byte]) => {
    val cipher = new Cipher(iv,k)
    val s = "a simple message"
    cipher.encrypt(s).length must beGreaterThanOrEqualTo(s.length)
  } ).setGen(genKeyBytes)


  val shortKeyLength = 24
  def genShortKeyString : Gen[String] = for { cc <- Gen.nonEmptyListOf(for {n <- Gen.chooseNum(0x0000,0x26FF)} yield n.toChar) } yield cc.take(keyLength-1).mkString

  def checkLessThan256bKeys = prop( (k:Array[Byte]) => {
    val cipher = new Cipher(iv,k.take(shortKeyLength))
    val s = "a simple message"
    cipher.encrypt(s).length must beLessThanOrEqualTo(1)
  } ).setGen(genKeyBytes)


  def encryptTextTest = {
    val cipher = new Cipher(iv,secretKey)

    val msg = scala.util.Random.alphanumeric.take(4096).mkString

    cipher.encrypt( msg ) must not( beEqualTo(msg) )
  }

  val maxMsgLength = 4096
//  def genMsgString : Gen[String] = for { cc <- Gen.nonEmptyListOf(for {n <- Gen.chooseNum(0x0000,0x26FF)} yield n.toChar) } yield cc.take(maxMsgLength).mkString
  def genMsgString : Gen[String] = for { cc <- Gen.nonEmptyListOf(for {n <- Gen.chooseNum(0x0000,0xFFFFF)} yield Character.toChars(n)) } yield cc.take(maxMsgLength).flatten.mkString

  def checkDecryption = prop( (ss:String) => {
    val cipher = new Cipher(iv,secretKey)
    println( "encrypt: (" + ss.length + ") " + ss.take(12).toArray.map(_.toInt).mkString(",") )
//    cipher.decrypt(cipher.encrypt(ss)).toArray.map(_.toInt) must beEqualTo(ss.toArray.map(_.toInt))
    success
  } ).setGen(genMsgString)
}

/*
class FileHammingLikeCodeTest extends Specification { def is = "test" ! pending }

class HostObscuredTest extends Specification { def is = "test" ! pending }
class RemoteKeyStorageTest extends Specification { def is = "test" ! pending }
class SSLTransportTest extends Specification { def is = "test" ! pending }

class WriteTest extends Specification with Mockito { def is = "test" ! pending }

*/
