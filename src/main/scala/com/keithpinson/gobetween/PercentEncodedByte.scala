package com.keithpinson.gobetween

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 8/27/2015
 */

/**
 * Encode or decode a given URL string using the Percent Encoding, supporting current URL style
 * encoding (space = %20) and application/x-www-form-urlencoded (space = +).
 *
 * @see [[com.keithpinson.gobetween.TermsAndConditions]]<br/>
 *
 * @author [[http://keithpinson.com Keith Pinson]]
 */
object PercentEncodedByte {
  private def toPctHex( c:Char ) : String = f"%%${c.toInt}%X"   // Hexadecimal should be upper case to match java.net.URLEncoder

  /**
   * Take an encoded URL and replace the percent encoded
   * @param url
   * @param isWwwForm
   * @return A decoded URL string
   */
  def decode( url:String, isWwwForm:Boolean = false ) : String = {
    var result : String = url

    if( ! isWwwForm ) {
      // Convert to www-form so that we can use java to decode

      result = result.
        replace(toPctHex(' '), "+").
        replace("~", toPctHex('~')).
        replace("!", toPctHex('!')).
        replace("$", toPctHex('$')).
        replace("%", toPctHex('%')).
        replace("&", toPctHex('&')).
        replace("(", toPctHex('(')).
        replace(")", toPctHex(')')).
        replace("+", toPctHex('+')).
        replace("'", toPctHex('\'')).
        replace(",", toPctHex(',') )
    }

    import java.io.UnsupportedEncodingException
    import java.lang.IllegalArgumentException
    try {
      result = java.net.URLDecoder.decode( result, "UTF-8")
    } catch {
      case e:UnsupportedEncodingException => url
      case e:IllegalArgumentException => url
      case e:Throwable => url
    }

    result
  }

  /**
   * Transform space and certain special characters to something more palatable to a web server.
   *
   * @param url A raw URL string
   * @param isWwwForm Indicates if the doctype is "application/x-www-form-urlencoded"
   * @return Encoded URL string
   */
  def encode( url:String, isWwwForm:Boolean = false ) : String = {
    var result = ""

    // Start with the java library...
    import java.io.UnsupportedEncodingException
    try {
      result = java.net.URLEncoder.encode( url, "UTF-8" )
    } catch {
      // What is the appropriate way to handle an exception:
      //   return the string with no encoding or return an empty string?
      case e: UnsupportedEncodingException => ""
    }

    if( ! isWwwForm ) {
      /* Space will have been converted to plus and of the special characters:
       *
       *   @#  ^      ` ={}|[]\:"; <>?  /     URL Special Characters Percent Encoded
       * ~!@#$%^& () +` ={}|[]\:";'<>?, /     Form Special Characters Percent Encoded
       * ~!  $% & () +            '   ,         These characters will have to be put back
       */

      result = result.
        replace("+", toPctHex(' ')).
        replace(toPctHex('~'), "~").
        replace(toPctHex('!'), "!").
        replace(toPctHex('$'), "$").
        replace(toPctHex('%'), "%").
        replace(toPctHex('&'), "&").
        replace(toPctHex('('), "(").
        replace(toPctHex(')'), ")").
        replace(toPctHex('+'), "+").
        replace(toPctHex('\''), "'").
        replace(toPctHex(','), ",")
    }

    result
  }
}
