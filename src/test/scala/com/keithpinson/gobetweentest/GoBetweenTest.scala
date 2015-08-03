package com.keithpinson.gobetweentest

/*
 * Copyright (c) 2015 Keith Pinson.
 *
 * Created: 7/20/2015
 */

import org.specs2.{Specification, SpecificationWithJUnit}
import org.specs2.specification.Snippets
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith


class GoBetweenTest extends SpecificationWithJUnit { def is = sequential ^
  "The Go Between Structure".title ^
  new EntityRefTest ^
  end
}
