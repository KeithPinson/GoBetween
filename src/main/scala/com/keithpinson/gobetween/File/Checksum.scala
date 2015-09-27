package com.keithpinson.gobetween.File

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/13/2015
 */

import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Date
import java.text.SimpleDateFormat

/**
 * This generates and verifies a "checksum" for a given string. We use the term
 * checksum when in fact, we may mean a hash (the specific implementation is
 * intentionally fluid.) A CRC checksum or some variant that can be defeated
 * by padding will not be used.
 *
 * This is a long checksum, 16 to 32 bytes or more.  It is not appropriate for strings
 * shorter than 256 bytes.
 *
 * To use properly: Encrypt first and then determine the checksum.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class Checksum( secretKey:String ) {

  // Current version should be at the head position
  final private val versions = scala.collection.SortedMap(
    'B'.toByte -> "2035-12-31T23:59:59Z",
    'A'.toByte -> "2000-01-01T00:00:00Z"  // Expired
  )

  private def hashB( bb:Array[Byte], ver:Char ) = {
    val mac = Mac.getInstance("HmacSHA256")

    mac.init(new SecretKeySpec(secretKey.getBytes("utf-8"), "HmacSHA256"))

    new String(ver.toByte +: mac.doFinal(bb))
  }


  def checksum( encryptedString:String ) : String = checksum( encryptedString.getBytes("utf-8") )

  def checksum( encryptedBytes:Array[Byte] ) : String = {
    val csVersion = versions.last._1

    csVersion match {
      case 'B' => hashB( encryptedBytes, 'B' )
      case _ => "A"
    }
  }

  def verify( encryptedString:String, cs:String ) : Boolean = verify( encryptedString.getBytes("utf-8"), cs )

  def verify( encryptedBytes:Array[Byte], cs:String ) : Boolean = ! isExpired( cs ) && checksum(encryptedBytes) == cs

  private def isExpired( cs:String ) : Boolean = {
    val csVersion = cs.head
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    val now = new Date(System.currentTimeMillis + 60 * 60 * 1000)

    csVersion match {
      case 'B' => dateFormat.parse(versions.getOrElse('B'.toByte, "2000-01-01T00:00:00Z")).compareTo(now) < 0
      case _ => true
    }
  }
}
