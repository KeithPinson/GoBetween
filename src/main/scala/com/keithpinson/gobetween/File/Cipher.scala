package com.keithpinson.gobetween.File

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/13/2015
 */

import java.security.Security
import javax.crypto.{SecretKey, KeyGenerator, Cipher=>xCipher}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import java.text.SimpleDateFormat
import java.util.Date
import org.bouncycastle.jce.provider.BouncyCastleProvider

/**
 * This is used for encrypting and decrypting strings using a block cipher.
 *
 * Note: the Initialization Vector should never be re-used.
 *
 * Some ciphers may not leak information if the IV is reused, but others are
 * worse and will leak it all. So to avoid a programming blunder, ensure that
 * the provided IV is always unique.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class Cipher( iv:String, secretKey:String ) {

  // Current version should be at the head position
  final private val versions = scala.collection.SortedMap(
    'B'.toByte -> "2035-12-31T23:59:59Z",
    'A'.toByte -> "2000-01-01T00:00:00Z"  // Expired
  )

  private def encryptB( plaintext:String, ver:Char ) = {

    // Add Bouncy Castle, will return -1 if it is added already
    Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider())

    val secretKeyObj = new SecretKeySpec(secretKey.getBytes("utf-8"), "HmacSHA256")
    val ivObj = new IvParameterSpec(iv.getBytes("utf-8"))

    val cipher = xCipher.getInstance("AES/CTR/NoPadding", BouncyCastleProvider.PROVIDER_NAME)
    cipher.init(xCipher.ENCRYPT_MODE, secretKeyObj, ivObj)

    new String( cipher.doFinal(plaintext.getBytes("utf-8")) )
  }

  def encrypt( plaintext:String ) : String = {
    val csVersion = versions.last._1

    plaintext.getBytes("utf-8")

    csVersion match {
      case 'B' => encryptB( plaintext, 'B' )
      case _ => "A"
    }
  }

  def decrypt( encryptedString:String ) : String = {
    if( ! isExpired( encryptedString ) ) {
      encryptedString.getBytes("utf-8")

      ""
    }
    else {
      ""
    }
  }

  private def isExpired( encryptedString:String ) : Boolean = {
    val csVersion = encryptedString.head
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    val now = new Date(System.currentTimeMillis + 60 * 60 * 1000)

    csVersion match {
      case 'B' => dateFormat.parse(versions.getOrElse('B'.toByte, "2000-01-01T00:00:00Z")).compareTo(now) < 0
      case _ => true
    }
  }



}
