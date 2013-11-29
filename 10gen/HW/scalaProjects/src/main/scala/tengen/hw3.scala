package tengen


import com.mongodb.casbah.Imports._
import org.slf4j.LoggerFactory

object hw3_1 extends App {
  val log = LoggerFactory.getLogger(hw3_1.getClass)
  val students = MongoClient()("school")("students")

  students.foreach(s => {
    val scores = s.as[Seq[BasicDBObject]]("scores")
    val minScore = scores.filter(_.getString("type") == "homework").map(_.getDouble("score")).min
    s.put("scores", scores.filterNot(score => score.getString("type") == "homework" && score.getDouble("score") == minScore))
    students.update(MongoDBObject("_id" -> s.get("_id")), s, false, false)
  })
}