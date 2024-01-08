import scala.collection.mutable

class FoodRatings(foods: Array[String], cuisines: Array[String], ratings: Array[Int]) {
    val cuisineOf = foods.iterator.zip(cuisines).toMap
    val ratingOf = foods.iterator.zip(ratings).to(mutable.Map)
    val rateMap = 
        ratings.view.map(- _).zip(foods).zip(cuisines)
        .groupMap(_._2)(_._1)
        .view.mapValues(_.to(mutable.TreeSet)).toMap


    def changeRating(food: String, newRating: Int) = {
        val oldRating = ratingOf(food)
        val rates = rateMap(cuisineOf(food))
        ratingOf(food) = -newRating
        rates -= (oldRating -> food)
        rates += (- newRating -> food)
    }

    def highestRated(cuisine: String): String = rateMap(cuisine).head._2
}