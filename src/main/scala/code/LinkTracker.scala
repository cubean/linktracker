/*                                                                      *\
**    Link Tracker programming test                           					**
**    https://github.com/cubean/linktracker.git               					**
\*                                                                      */
package code

import scala.xml._

import scala.language.postfixOps
import javax.xml.stream.XMLStreamException

/**
 * Link Tracking
 *
 * Given an HTML document as Scala XML:
 *
 * For each a element with an href attribute or img element with a src attribute
 * in the document if the attribute value contains "cust" but not "internal"
 * replace it with a unique numeric ID.  The IDs should start at 0 and increment
 * by 7.  If the path matches one we've already seen reuse the old ID.
 *
 * Return the updated HTML document along with a Map of IDs to paths wrapped in
 * a PathTrackerResult.
 * 

 * Coding by Cubean Liu at 07/05/2016. Modified at 20/05/2016
 * Any questions, please contact with email: cubean@outlook.com
 *
 */
object LinkTracker {

  /**
   * a xml document parser function
   */
  def parseDoc(doc: Elem): PathTrackerResult = {

    object counter {
      private var initVal = -7
      def value = { initVal += 7; initVal }
    }

    var encodedPathsMap: Map[Int, String] = Map()

    def idGetter(attValue: String) = {

      var ret: Option[(Int, String)] = encodedPathsMap.find(p => p._2.compareTo(attValue) == 0)
      if (ret isEmpty) {
        var numVal = counter.value
        encodedPathsMap += (numVal -> attValue)
        numVal
      } else ret.get._1

    }

    // Set could ensure the unique strings
    var uniquePathsSet: Set[String] = Set()

    var totalPaths = 0;

    //an iterated function to find matched link with original order 
    def trackNode(node: NodeSeq): NodeSeq = node match {
      case <html>{ ns @ _* }</html> => {
        <html>{ ns.map(trackNode) }</html>
      }
      case <body>{ ns @ _* }</body> => {
        <body>{ ns.map(trackNode) }</body>
      }
      case x @ <a>{ ns @ _* }</a> => {
        totalPaths += 1
        var attValue = (x \ "@href").toString()
        uniquePathsSet += attValue
        if (attValue.contains("cust") && !attValue.contains("internal")) {
          XML.loadString("<a href=\"" + idGetter(attValue) + "\">" + ns.text + "</a>")
        } else x
      }
      case x @ <img/> => {
        totalPaths += 1
        var attValue = (x \ "@src").toString()
        uniquePathsSet += attValue
        if (attValue.contains("cust") && !attValue.contains("internal")) {
          XML.loadString("<img src=\"" + idGetter(attValue) + "\"/>")
        } else x
      }
      case other @ _ => other
    }

    var trackedDoc: NodeSeq = <html/>

    try {
      trackedDoc = trackNode(doc)
      println(trackedDoc);

    } catch {
      case _: XMLStreamException => println("Bad xml: " + doc)
      case ex: Exception         => ex.printStackTrace()
    }

    PathTrackerResult(trackedDoc.toString(), encodedPathsMap, uniquePathsSet.size, totalPaths)
  }
}

case class PathTrackerResult(trackedDoc: String, encodedPaths: Map[Int, String],
                             uniquePaths: Int, totalPaths: Int) 
