package dlmScala
// https://futures.io/reviews-brokers-data-feeds/31385-google-finance-historical-daily-data-retrieved-programmatically.html

object GetStockData {
  import java.io.{BufferedReader, InputStreamReader, PrintWriter, File}
  import java.net.{URL, URLConnection}

  val stocks = List("MCD", "CL", "ORCL", "WFC", "COST")
  // Costco is missing 1 April, 2016... Forget it...
  //val stocks = List("MCD", "CL", "ORCL", "WFC")
  val template = "http://www.google.com/finance/historical?q=TICKER&histperiod=daily&startdate=Jan+1+2010&enddate=Jan+1+2017&output=csv"

  def getTickerInfo(ticker: String): List[String] = {
    val url = new URL(template.replace("TICKER", ticker))
    val urlConn = url.openConnection()
    val inputStreamReader = new InputStreamReader(urlConn.getInputStream)
    val bufferedReader = new BufferedReader(inputStreamReader)

    def loop(acc: List[String]=List()): List[String] = {
      bufferedReader.readLine() match {
        case line: String => loop(line :: acc)
        case null => {
          bufferedReader.close()
          inputStreamReader.close()
          acc.last :: acc.dropRight(1)
        }
      }
    }

    loop()
  }

  def go() = {
    val dir = "src/main/resources/"
    stocks.foreach(ticker => {
      val pw = new PrintWriter(new File(dir + ticker + ".csv" ))
      val lines = getTickerInfo(ticker)
      //lines.foreach(line => pw.write(line + "\n"))
      pw.write(lines.mkString("\n"))
      pw.close
    })
  }

}
