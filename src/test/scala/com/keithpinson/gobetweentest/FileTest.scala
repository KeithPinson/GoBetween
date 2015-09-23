package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 9/13/2015
 */

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

class FileRequestTest extends Specification { def is = pending }
class ReadSecurityTest extends Specification { def is = pending }
class ReadHandlerTest extends Specification { def is = pending }

class ReadCacheReservationTest extends Specification { def is = pending }
class ReadCachePrioritiesTest extends Specification { def is = pending }
class ReadFileLocalStorageTest extends Specification { def is = pending }
class ReadCachePruningTest extends Specification { def is = pending }

class ReadFileStreamTest extends Specification { def is = failure }
class ReadLiveUpdatesTest extends Specification { def is = pending }


class WriteTest extends Specification with Mockito { def is =
  pending


}
