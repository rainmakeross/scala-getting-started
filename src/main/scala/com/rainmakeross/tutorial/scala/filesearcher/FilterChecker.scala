package com.rainmakeross.tutorial.scala.filesearcher

import java.io.File
import javax.tools.FileObject

import scala.util.control.NonFatal


class FilterChecker(filter:String) {
  val filterAsRegEx = filter.r

  def matches(content:String) = filterAsRegEx findFirstMatchIn content match {
    case Some(_) => true
    case None => false
  }

  def findMatchedFiles(ioObjects:List[IOObject]) =
    for(iOObject <- ioObjects
      if(iOObject.isInstanceOf[FileObject])
      if(matches(iOObject.name)))
    yield iOObject

  def findMatchedContentCount(file: File) = {
    def getFilterMatchCount(content:String) =
      (filterAsRegEx findAllIn content).length
    import scala.io.Source
    try{
      val fileSource = Source.fromFile(file)
      fileSource.getLines().foldLeft(0)(
        (accumulator, line) => accumulator + getFilterMatchCount(line))

    }
    catch {
      case NonFatal(_) => 0
    }
  }
}

object FilterChecker {
  def apply(filter:String) = new FilterChecker(filter)
}
