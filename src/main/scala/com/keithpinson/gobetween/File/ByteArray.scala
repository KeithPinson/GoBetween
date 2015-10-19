package com.keithpinson.gobetween.File

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 10/19/2015
 */

/**
 * This trait's only purpose is to support a higher type that can be
 * used to know just what the bytes in the array represent: a type of
 * character encoding, a cipher using a particular encryption, etc.
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
trait ByteArray {
  val higherType : String
}
