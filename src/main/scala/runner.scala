import java.net.{URL, URLConnection}

import org.jsoup.Jsoup
import org.jsoup.select.Elements

import scala.annotation.tailrec
import scala.util.Try
import PreviewMaker._

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

  def getPreview(url: String): Map[String, String] = {
    val jUrl = Try(new URL(url))
    if (jUrl.isSuccess) {
      var map = Map.empty[String, String]
      val document = Jsoup.connect(url).get

      @tailrec
      def addToMap(metaTags: List[MetaTag]): Map[String, String] = metaTags match {
        case l :: ls =>
          val tag = document.select(s"meta[property=og:${l.tagName}]")
          map += l.tagName -> tag.attr("content")
          addToMap(ls)
        case Nil =>
          map.filter(p => p._2.length > 0)
      }

      var result = addToMap(metaTagsList)
      result.get(Image.tagName) match {
        case Some(imgUrl) =>
          val imageUrl = dropSlash(imgUrl)
          if (Try(new URL(imageUrl).getContent).isFailure) {
            if (Try(new URL(http + imageUrl).getContent).isFailure) {
              if (Try(new URL(https + imageUrl).getContent).isFailure) {
                result += Image.tagName -> (jUrl.get.getProtocol + "://" + jUrl.get.getHost + "/" + imageUrl)
              } else {
                result += Image.tagName -> (https + imageUrl)
              }
            } else {
              result += Image.tagName -> (http + imageUrl)
            }
          } else {
            val firstImageUrl = getFirstImage(url)
            if (firstImageUrl != "")
              result += Image.tagName -> firstImageUrl
          }
        case _ =>
          val firstImageUrl = getFirstImage(url)
          if (firstImageUrl != "")
            result += Image.tagName -> firstImageUrl
      }
      result
    }
    else Map.empty
  }
}


object main extends App {
  val url = "https://www.instagram.com/cristiano/?hl=en"
  val urlList = List(
    "http://www.varzesh3.com")


  urlList.foreach { url =>
    val result = PreviewMaker.getPreview(url)
    println(s"****************************** url: ${url} ***************************************************")
    val str = result.get("image").get
    println("size : " + getImageSize(str))
    println("url : " + str)
    println
  }


}
