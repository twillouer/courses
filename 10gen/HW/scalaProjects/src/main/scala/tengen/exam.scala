package tengen

import com.mongodb.casbah.Imports._
import com.mongodb.{casbah, BasicDBObject}

object exam extends App {
  val albums = MongoClient()("courses")("albums")

  val imagesId: Iterable[Integer] = albums.map(_.as[Seq[Integer]]("images")).flatMap(a => a)
//  println(imagesId)

  val images = MongoClient()("courses")("images")

  val criteria = ("_id" $not { _ $in imagesId })
//  println(criteria)
  images.remove(criteria)

  //   db.images.find({tags:"sunrises"}).count();

  val count = images.count(MongoDBObject("tags" -> "sunrises"))
  println(s"Count : $count")

//  students.foreach(s => {
//    val scores = s.as[Seq[BasicDBObject]]("scores")
//    val minScore = scores.filter(_.getString("type") == "homework").map(_.getDouble("score")).min
//    s.put("scores", scores.filterNot(score => score.getString("type") == "homework" && score.getDouble("score") == minScore))
//    students.update(MongoDBObject("_id" -> s.get("_id")), s, false, false)
//  })
}


/**
 * User: william
 * Date: 26/06/13
 */
object Question8 {
  def main(args: Array[String]) {
    val animals: casbah.MongoCollection = MongoClient()("test")("animals")

    val animal = new BasicDBObject("animal", "monkey")
    animals.insert(animal)
    animal.removeField("animal")
    animal.append("animal", "cat")
    animals.insert(animal)
    animal.removeField("animal")
    animal.append("animal", "lion")
    animals.insert(animal)
  }
}


