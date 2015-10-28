package com.keithpinson.gobetween.File

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/13/2015
 */

import java.security.Security
import javax.crypto.{Cipher => xCipher, _}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import java.text.SimpleDateFormat
import java.util.Date
import javax.management.openmbean.InvalidKeyException
import com.typesafe.scalalogging


/**
 * This is used for encrypting and decrypting strings using a block cipher.
 *
 * Note: the Initialization Vector should be one that is never re-used.
 *
 * Some ciphers leak no information if the IV is reused, but others are much
 * worse and will leak it all if reused. So, to avoid a programming blunder,
 * you must ensure that the provided IV is unique.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
class Cipher( iv:Array[Byte], secretKey:Array[Byte] ) {

  // Current version should be at the head position
  final private val versions = scala.collection.SortedMap(
    'B'.toByte -> "2035-12-31T23:59:59Z",
    'A'.toByte -> "2000-01-01T00:00:00Z"  // Expired
  )

  private def cipherB( isEncrypted:Boolean, s:String, ver:Char ) : String = {

    import org.bouncycastle.jce.provider.BouncyCastleProvider

    // Short-circuit if the keylength is bad
    if( secretKey.length < 32 ) return "A"

    // No harm calling this repeatedly since Add Bouncy Castle ("BC") will just return -1 if it is added already
    Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider())

//    val secretKeyObj = new SecretKeySpec(secretKey.take(16), "HmacSHA256")
    val secretKeyObj = new SecretKeySpec(secretKey, "HmacSHA256")

//    val secretKeyObj = KeyGenerator.getInstance("AES").generateKey
    val ivObj = new IvParameterSpec(iv)

    val cipher = xCipher.getInstance("AES/CTR/NoPadding", BouncyCastleProvider.PROVIDER_NAME)

    try {
      cipher.init(if (isEncrypted) xCipher.DECRYPT_MODE else xCipher.ENCRYPT_MODE, secretKeyObj, ivObj)
    }
    catch {
      case e:java.security.InvalidKeyException => System.err.println(">"*5 + " key length: " + secretKey.length * 8 + ", max allowed: " + xCipher.getMaxAllowedKeyLength("AES") + "\n\nJava Cryptography Extension (JCE) is not installed\n\n" + scala.util.Properties.jdkHome + "\n\n" ); throw e   // Caused be not having JCE (Java Cryptography Extension) Unlimited Strength installed
    }

//    val utf = new Utf8

    try {
      if (isEncrypted)
//UTF-8 doesn't work in all cases
//encode to base64 ?
//        new String(utf.decodeFromUTF8(cipher.doFinal(s.tail.getBytes("UTF8"))))  // Decrypt just the tail
//      else
//        new String(ver.toByte +: cipher.doFinal(utf.encodeToUTF8(s)))             // Encrypt and add ver as the head
//        new String(cipher.doFinal(s.tail),"UTF16")  // Decrypt just the tail
        new String()
      else
        new String(ver.toByte +: cipher.doFinal(s.getBytes("UTF16")))             // Encrypt and add ver as the head
    }
    catch {
      case e:IllegalBlockSizeException => throw e //"A"
      case e:BadPaddingException => throw e //"A"
    }
  }

  def encrypt( plaintext:String ) : String = {
    val csVersion = versions.last._1

    csVersion match {
      case 'B' => cipherB( isEncrypted = false, plaintext, 'B' )
      case _ => "A"
    }
  }

  def decrypt( encryptedString:String ) : String = {

    if( ! isExpired( encryptedString ) ) {

      val csVersion = encryptedString.head

      csVersion match {
        case 'B' => cipherB( isEncrypted = true, encryptedString, 'B' )
        case _ => "A"
      }
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
