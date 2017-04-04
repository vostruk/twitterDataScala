package rest


import org.apache.http.client.HttpClient
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import oauth.signpost.commonshttp.CommonsHttpOAuthConsumer
import org.apache.commons.io.IOUtils
import rest.utils.FileSupport
import com.typesafe.config.ConfigFactory
import java.io.PrintWriter
import scala.util.parsing.json._
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;


object JustDownload extends App with FileSupport {
 
	  val  ConsumerKey  =  "9DZO2bQPgmXO4r2eML5yVE7tb";
	  val  ConsumerSecret  = "XgYcclHj3WPIvRa8GAzxNCT630D7yPW7ywxlcsDNguq7G0AUSW";
	  val AccessToken = "1147364532-UY07fDELfbBmIY6D1Fghf80BEO28ik683MKYry0";
	  val AccessSecret = "lLOedCO9h9Zfqym41xAk9RR0r2erO4YgNVLKY0SXp0x5x";
 


 def parse(jsonString: String) {

	class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

	object M extends CC[Map[String, Any]]
	object L extends CC[List[Any]]
	object S extends CC[String]
	object D extends CC[Double]
	object B extends CC[Boolean]


	val result = for {
	    Some(M(map)) <- List(JSON.parseFull(jsonString))
	    L(statuses) = map("statuses")
	    M(tweet) <- statuses
	    S(text) = tweet("text")
	    S(created) = tweet("created_at")
	    D(id) = tweet("id")
	} yield {
	    (text, created, id)
	}
 
	println(result.size)


         val st = result.map { tuple =>   tuple.productIterator.mkString("\t")}
	

	new PrintWriter("myListOfTweets.txt") { write(st mkString("\n")); close }
    //result
}





  def search(keywords : String, dateFrom : String, dateTo : String) {
 
	 val consumer = new CommonsHttpOAuthConsumer(ConsumerKey,ConsumerSecret);
	 consumer.setTokenWithSecret(AccessToken, AccessSecret);
 	
	var sinceStr = ""
	var untilStr = ""
	
	if(dateFrom!="") sinceStr=" since:"+dateFrom
	if(dateTo!="") untilStr=" until:"+dateTo

	val str=  keywords + sinceStr  + untilStr;
	val s = URLEncoder.encode(str, "UTF-8");
	println(s) 

     val request = new HttpGet("https://api.twitter.com/1.1/search/tweets.json?q="+s+"&count=100" ); // + "%23brexit" + "%20since%3A"+dateFrom  +"%20until%3A"+dateTo);
     
     consumer.sign(request);

     val client = new DefaultHttpClient();
     val response = client.execute(request);
 
     println(response.getStatusLine().getStatusCode());
     val jsonRes = IOUtils.toString(response.getEntity().getContent())
    // println();

  val filename = {
    val config = ConfigFactory.load()
    config.getString("tweets.scalax")
  }


    new PrintWriter("myJSON.txt") { write(jsonRes); close }
	
    //FileUtils.readFileToString(file, StandardCharsets.UTF_8)

    println( parse(jsonRes))


  }
 
 print("Query: ");
 //"#brexit"
 val q = scala.io.StdIn.readLine()

 //"2017-03-31"
 print("From (ex. 2017-03-31): ");
 val f = scala.io.StdIn.readLine()

 //"2017-04-01"
 print("To (ex. 2017-04-01): ");
 val t = scala.io.StdIn.readLine()
 search(q,f, t )

}
