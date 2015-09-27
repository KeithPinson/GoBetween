package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/13/2015
 */

import com.keithpinson.gobetween.File._
import javax.crypto.KeyGenerator
import org.specs2.Specification
import org.specs2.mock.Mockito

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
  "fail" ! failure ^
  "And, a file writer" ^ {new WriteTest} ^
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
  "A contract that the file will not be removed during downloads/live updates" ^ {new ReadCacheReservationTest}
  "A configurable system to prioritize a file's place in the cache" ^ {new ReadCachePrioritiesTest}
  "A mechanism to date files, obscure local file names, and record the file request" ^ {new ReadFileLocalStorageTest}
  "Removal of stale files and low priority files" ^ {new ReadCachePruningTest} ^
  end
}

class ReadListenerTest extends Specification { def is =
  "Read Listener Test".title ^
  "All files are assumed to be transmitted from a remote system.A listener exists to support:" ^
  "Remotely streamed files" ^ {new ReadFileStreamTest} ^
  "Live updates to a cached file" ^ {new ReadLiveUpdatesTest} ^
  end
}

class FileRequestTest extends Specification { def is =
  "File Request Test".title ^
  "To avoid resending files that exist in the cache a file request consists of a header with the following metadata:" ^
  "File URL" ^ {new FileURLTest} ^
  "File Name" ^ {new FileNameTest} ^
  "Metadata Version" ^ {new MetadataVersionTest} ^
  "Packets in cache" ^ {new FileSpanningTest} ^
  "Checksum of file" ^ {new FileChecksumTest} ^
  "Hamming-like Code" ^ {new FileHammingLikeCodeTest} ^
  end
}

class ReadSecurityTest extends Specification { def is =
  "Read Security Test".title ^
  "To try and avoid security nightmares, the request of a file should be treated like an API request, where:" ^
  "Server host is obscured" ^ {new HostObscuredTest} ^
  "API (file server) key stored remotely" ^ {new RemoteKeyStorageTest} ^
  "SSL (https) is used" ^ {new SSLTransportTest} ^
  end
}

class ReadHandlerTest extends Specification { def is =
  "Read Handler Test".title ^
  "The read handler is responsible for assebling the received packets and passing them to a parser for conversion to the goBetween format format" ^
  "test" ! pending ^
  end
}

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

class FileChecksumTest extends Specification { def is = s2"""
  A string of characters is encrypted and then a hash, ie. checksum, is calculated $genChecksumTest
  The checksum should support versionioning
    Versions should have an expiry mechanism $versioningTest
  An existing checksum can be compared against a string of characters $checksumVerifyTest
"""

  val msg = scala.util.Random.alphanumeric.take(4096).mkString

  def genChecksumTest = {
    val keyGenerator = KeyGenerator.getInstance("HmacMD5")
    val secretKey = new String( keyGenerator.generateKey().getEncoded )

    val cs = new Checksum( secretKey )

    val hash = cs.checksum(msg)

    hash must beAnInstanceOf[String]
  }

  def versioningTest = {
    failure
  }

  def checksumVerifyTest = {
    failure
  }
}

class FileHammingLikeCodeTest extends Specification { def is = "test" ! pending }

class HostObscuredTest extends Specification { def is = "test" ! pending }
class RemoteKeyStorageTest extends Specification { def is = "test" ! pending }
class SSLTransportTest extends Specification { def is = "test" ! pending }


class WriteTest extends Specification with Mockito { def is = "test" ! pending }
