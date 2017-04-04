package rest

import com.danielasfregola.twitter4s.TwitterRestClient
import com.danielasfregola.twitter4s.entities.Tweet
import com.danielasfregola.twitter4s.entities.enums.ResultType
import com.typesafe.config.ConfigFactory
import rest.utils.FileSupport
import java.io.PrintWriter
import scala.util.{Success, Failure}
import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import com.danielasfregola.twitter4s.entities.{HashTag, AccessToken, ConsumerToken}

import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.{ ask, pipe }

    class TweetProcessor extends Actor {
     
         
      def receive = {
        case result : Future[Seq[Tweet]]  => {

          result.onComplete({
    	    case Success(tweets) => {
      
	       println(s"Downloaded ${tweets.size} tweets")
	  
		 val hashtags: Seq[String ] = tweets.map { tweet =>
		       tweet.text
			
		    }

		//val hashtagTexts: Seq[String] = hashtags.flatten.map(_.text.toLowerCase)
		new PrintWriter("myJSONBig.txt") { write(hashtags mkString("\n")); close }
	//	toFileAsJson(filename, tweets)
		
		//here we can send tweets to the next actor or in our case we continue the loop
		context.parent ! "nextInput"
		
	    }
	    case Failure(exception) => {
	     
		println(exception)
	    }
	  })

	
	}
        
	case "done" =>
          println("done")
      }
     
    }



class TweetDownloader extends Actor {
  
  val client = TwitterRestClient()//consumerToken, accessToken)

  def searchTweets(i: Int, query: String, max_id: Option[Long] = None): Future[Seq[Tweet]] = {
    def extractNextMaxId(params: Option[String]): Option[Long] = {
      //example: "?max_id=658200158442790911&q=%23scala&include_entities=1&result_type=mixed"
      params.getOrElse("").split("&").find(_.contains("max_id")).map(_.split("=")(1).toLong)
    }
    
    client.searchTweet(query, count = 100, result_type = ResultType.Mixed, max_id = max_id).flatMap { ratedData =>
        val result = ratedData.data
        val nextMaxId = extractNextMaxId(result.search_metadata.next_results)
        val tweets = result.statuses
	
	println(result.search_metadata.query)
	println(result.search_metadata.count)
	println(nextMaxId)
	println(tweets.size)
	

	if (tweets.nonEmpty && i>0) searchTweets(i-1, query, nextMaxId).map(_ ++ tweets)
        else Future(tweets.sortBy(_.created_at))
      } recover { case _ => Seq.empty }
  }




  def receive = {
    case (num: Int, tag: String, t:ActorRef) => {
 	 println("processing tweets (download)")
	 val result = searchTweets(num, tag)//.map { tweets =>
	 //  sender ! tweets }
	 t ! result
	}

    case "hello" => println("buenos dias!")
    case _       => println("huh?")
  }
}

class Looper extends Actor {
     
  val TDownloader = context.actorOf(Props[TweetDownloader], name = "DownloadActor")
  val TProcessor = context.actorOf(Props[TweetProcessor], name = "ProActor")
 
      def receive = {

	case "nextInput" => {
	   print("Query: ");
 		//"#brexit"
 	   val q = scala.io.StdIn.readLine()
	  if(q!="")
           TDownloader  ! (1, q, TProcessor) //%20since%3A2017-04-02%20until%3A2017-04-04
	  else self ! "done"
	}        
	case "done" =>
          println("done")
	context.system.shutdown()
      }
     
    }
object SearchAndSaveTweets extends App with FileSupport {

  val system = ActorSystem("DownloadSystem")
  val looperActor = system.actorOf(Props[Looper], name = "looperActor")
  
  looperActor ! "nextInput"

}
