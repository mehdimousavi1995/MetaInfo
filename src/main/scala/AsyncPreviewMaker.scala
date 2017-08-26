
import java.net.{URL}

import org.jsoup.Jsoup
import org.jsoup.select.Elements
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import org.jsoup.nodes.Document
import scala.concurrent.Future
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


  object Topic extends MetaTag {
    override val tagName: String = "topic"
  }

  val metaTagsList: List[MetaTag] = List(Image, Title, Description, Topic)

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


  def getFirstImage(url: String): Future[String] = {
    def getFirstLink(elements: List[String]): String = elements match {
      case l :: ls =>
        if ((Try(new URL(l).getContent).isFailure || l.split("\\.").contains("gif")) || getImageSize(l) < minSize)
          getFirstLink(ls)
        else l
      case Nil => ""
    }

    val images: Future[Elements] = Future(Jsoup.connect(url).get().select("body").select("img"))
    images.map { result =>
      var elementsList = List.empty[String]
      result.forEach { element => elementsList = element.absUrl("src") :: elementsList }
      getFirstLink(elementsList)
    }.recover {
      case e: Throwable =>
        // log.warning("failed to get image link from {}, caused by: {}", url, e.getMessage)
        ""
    }
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

      result.flatMap { map =>
        val imgUrl = map.get(Image.tagName).getOrElse("-1")
        if (imgUrl != "-1" && !imgUrl.split("\\.").contains("gif")) {
          val imageUrl = dropSlash(imgUrl)
          if (Try(new URL(imageUrl).getContent).isFailure)
            if (Try(new URL(http + imageUrl).getContent).isFailure)
              if (Try(new URL(https + imageUrl).getContent).isFailure)
                result map (m => m ++ Map(Image.tagName -> (jUrl.get.getProtocol + "://" + jUrl.get.getHost + "/" + imageUrl)))
              else
                result map (m => m ++ Map(Image.tagName -> (https + imageUrl)))
            else
              result map (m => m ++ Map(Image.tagName -> (http + imageUrl)))
          else
            result
        } else
          getFirstImage(url) flatMap { firstImgUrl =>
            result.map(m => m ++ Map(Image.tagName -> firstImgUrl))
          }
      }.recover {
        case e: Throwable =>
          // log.warning("failed to get preview link from {}, caused by: {}", url, e.getMessage)
          Map.empty
      }

    } else Future.successful(Map.empty)
  }

}

object runner extends App {

  val previewMaker = PreviewMaker.getPreview("http://news.nationalgeographic.com/2017/08/ancient-babylonian-trigonometry-tablet-plimpton-322-video-spd/")
  previewMaker onComplete {
    case Success(s) =>
      println(s.toString)
    case Failure(f) => println(f.getMessage)
  }


  Thread.sleep(60000)
}

