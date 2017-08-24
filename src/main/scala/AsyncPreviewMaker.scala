
import java.net.{URL, URLConnection}

import org.jsoup.Jsoup
import org.jsoup.select.Elements

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
//import PreviewMaker._
import akka.actor.ActorSystem
import akka.util.Timeout
import org.jsoup.nodes.Document

import scala.concurrent.Future
import scala.concurrent.forkjoin._

// the following is equivalent to `implicit val ec = ExecutionContext.global`
import scala.concurrent.ExecutionContext.Implicits.global

object PreviewMaker {


  val http = "http://"
  val https = "https://"

  val minSize = 3000

  trait MetaTag {
    val tagName: String
  }

  object Image extends MetaTag {
    override val tagName: String = "image"
  }

  object Title extends MetaTag {
    override val tagName: String = "title"
  }

  object Description extends MetaTag {
    override val tagName: String = "description"
  }

  object Keywords extends MetaTag {
    override val tagName: String = "keywords"
  }

  object Subject extends MetaTag {
    override val tagName: String = "subject"
  }

  object Topic extends MetaTag {
    override val tagName: String = "topic"
  }

  val metaTagsList: List[MetaTag] = List(Image, Title, Description, Keywords, Subject, Topic)

  def dropSlash(host: String): String = {
    if (host.startsWith("/"))
      dropSlash(host.substring(1))
    else host
  }

  def getImageSize(url: String): Int = {
    val jUrl = new URL(url)
    val urlConnection = jUrl.openConnection
    urlConnection.getContentLength
  }


  def getFirstImage(url: String): String = {
    val doc = Jsoup.connect(url).get()
    val imgs: Elements = doc.select("body").select("img")
    var elementsList = List.empty[String]
    imgs.forEach { element => elementsList = element.absUrl("src") :: elementsList }

    def getFirstLink(elements: List[String]): String = elements match {
      case l :: ls =>
        if ((Try(new URL(l).getContent).isFailure || l.split("\\.").contains("gif")) || getImageSize(l) < minSize)
          getFirstLink(ls)
        else l
      case Nil => ""
    }

    getFirstLink(elementsList)
  }

  def getPreview(url: String): Future[Map[String, String]] = {
    val jUrl = Try(new URL(url))
    if (jUrl.isSuccess) {
      @tailrec
      def buildPreview(metaTags: List[MetaTag], doc: Document, intermediateResult: Map[String, String]): Map[String, String] = metaTags match {
        case l :: ls =>
          val tag = doc.select(s"meta[property=og:${l.tagName}]")
          buildPreview(ls, doc, intermediateResult ++ Map(l.tagName -> tag.attr("content")))
        case Nil =>
          intermediateResult
      }
      val document = Future(Jsoup.connect(url).get)
      val result: Future[Map[String, String]] = document.map { doc =>
        buildPreview(metaTagsList, doc, Map.empty).filter(_._2.size > 0)
      }
      result
    } else Future.successful(Map.empty)
  }

}

object runner extends App {

  val previewMaker = PreviewMaker.getPreview("http://www.nationalgeographic.com/")
  previewMaker onComplete {
    case Success(s) =>
      println(s.toString)
    case Failure(f) => println(f.getMessage)
  }

  Thread.sleep(4999)
}

