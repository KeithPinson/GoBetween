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

  private def cipherB( isEncrypted:Boolean, bb:Array[Byte], ver:Char ) : Array[Byte] = {

    import org.bouncycastle.jce.provider.BouncyCastleProvider

    val emptyResult = if( isEncrypted ) Array[Byte]() else "A".getBytes

    // Short-circuit if the keylength is bad
    if( secretKey.length < 32 ) return emptyResult

    // No harm calling this repeatedly since Add Bouncy Castle ("BC") will just return -1 if it is added already
    Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider())

//    val secretKeyObj = new SecretKeySpec(secretKey.take(16), "HmacSHA256")
    val secretKeyObj = new SecretKeySpec(secretKey, "HmacSHA256")

    val ivObj = new IvParameterSpec(iv)

    val cipher = xCipher.getInstance("AES/CTR/NoPadding", BouncyCastleProvider.PROVIDER_NAME)

    try {
      cipher.init(if(isEncrypted) xCipher.DECRYPT_MODE else xCipher.ENCRYPT_MODE, secretKeyObj, ivObj)
    }
    catch {
      case e:java.security.InvalidKeyException => System.err.println(">"*5 + " key length: " + secretKey.length * 8 + ", max allowed: " + xCipher.getMaxAllowedKeyLength("AES") + "\n\nJava Cryptography Extension (JCE) is not installed\n\n" + scala.util.Properties.jdkHome + "\n\n" ); throw e   // Caused be not having JCE (Java Cryptography Extension) Unlimited Strength installed
    }

    try {
      if(isEncrypted)
        cipher.doFinal(bb.tail)           // Decrypt just the tail
      else
        ver.toByte +: cipher.doFinal(bb)  // Encrypt and add ver as the head
    }
    catch {
      case e:IllegalBlockSizeException => /*throw e*/ emptyResult
      case e:BadPaddingException => /*throw e*/ emptyResult
    }
  }

  def encrypt( plaintextBytes:Array[Byte] ) : Array[Byte] = {
    val csVersion = versions.last._1

    csVersion match {
      case 'B' => cipherB( isEncrypted = false, plaintextBytes, 'B' )
      case _ => "A".getBytes
    }
  }

  def decrypt( encryptedBytes:Array[Byte] ) : Array[Byte] = {

    if( ! isExpired( encryptedBytes ) ) {

      val csVersion = encryptedBytes.head

      csVersion match {
        case 'B' => cipherB( isEncrypted = true, encryptedBytes, 'B' )
        case _ => Array[Byte]()
      }
    }
    else {
      Array[Byte]()
    }
  }

  private def isExpired( encryptedBytes:Array[Byte] ) : Boolean = {
    val csVersion = encryptedBytes.head
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    val now = new Date(System.currentTimeMillis + 60 * 60 * 1000)

    csVersion match {
      case 'B' => dateFormat.parse(versions.getOrElse('B'.toByte, "2000-01-01T00:00:00Z")).compareTo(now) < 0
      case _ => true
    }
  }



}
