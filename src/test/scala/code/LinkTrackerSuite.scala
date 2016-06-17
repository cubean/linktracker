package code

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

/**
  * for test
  */
@RunWith(classOf[JUnitRunner])
class LinkTrackerSpec extends FlatSpec with Matchers {
  val res1 = LinkTracker.parseDoc(Sample1.originalDoc)

  "The LinkTracker" should "count the total number of img/a paths in a document" in {
    res1.totalPaths should be (Sample1.expectedTotalPaths)
  }

  it should "count the total unique img/a paths in a document" in {
    res1.uniquePaths should be (Sample1.expectedUniquePaths)
  }

  it should "provide a set of encoded paths for a document as a map of encoding id to path" in {
    res1.encodedPaths should be (Sample1.expectedEncodedPaths)
  }

  it should "provide a String version of the document with all img/a paths encoded" in {
    res1.trackedDoc should be (Sample1.expectedDoc)
  }

  val res2 = LinkTracker.parseDoc(Sample1.originalDoc)

  "The LinkTracker2" should "count the total number of img/a paths in a document" in {
    res2.totalPaths should be (Sample1.expectedTotalPaths)
  }

  it should "count the total unique img/a paths in a document" in {
    res2.uniquePaths should be (Sample1.expectedUniquePaths)
  }

  it should "provide a set of encoded paths for a document as a map of encoding id to path" in {
    res2.encodedPaths should be (Sample1.expectedEncodedPaths)
  }

  it should "provide a String version of the document with all img/a paths encoded" in {
    res2.trackedDoc should be (Sample1.expectedDoc)
  }
  
}

object Sample1 {
  val originalDoc =
<html>
  <head>
    <title>Some Doc</title>
    <link rel="stylesheet" href="/styles/mystyle.css"/>
    <link rel="stylesheet" href="/styles/custstyles/mystyle.css"/>
    <style type="text/css">
      {""".blah { background-image: url('/img/happycustomers.jpg'); }"""}
    </style>
  </head>
  <body>
    Some intro text about mypic and something.
    <a href="/something/about/customers">GO HERE</a>
    <img src="/img/mypic.jpg"/>
    <img src="/img/internalcustonly/mypic.jpg"/>
    <img src="/img/happycustomers.jpg"/>
    <img src="/img/mypic.jpg"/>
    <a href="/something/about/customers">OR HERE</a>
  </body>
</html>

  val expectedEncodedPaths = Map(
    0 -> "/something/about/customers",
    7 -> "/img/happycustomers.jpg"
  )

  val expectedUniquePaths = 4

  val expectedTotalPaths = 6

  val expectedDoc =
"""<html>
  <head>
    <title>Some Doc</title>
    <link rel="stylesheet" href="/styles/mystyle.css"/>
    <link rel="stylesheet" href="/styles/custstyles/mystyle.css"/>
    <style type="text/css">
      .blah { background-image: url('/img/happycustomers.jpg'); }
    </style>
  </head>
  <body>
    Some intro text about mypic and something.
    <a href="0">GO HERE</a>
    <img src="/img/mypic.jpg"/>
    <img src="/img/internalcustonly/mypic.jpg"/>
    <img src="7"/>
    <img src="/img/mypic.jpg"/>
    <a href="0">OR HERE</a>
  </body>
</html>"""
}


