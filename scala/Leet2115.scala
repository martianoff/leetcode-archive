import scala.collection.mutable

object Leet2115 extends App {
  object Solution {
    def findAllRecipes(recipes: Array[String], ingredients: List[List[String]], supplies: Array[String]): List[String] = {
      // build list of recipes with requirements
      val ingredientsToRecipes = mutable.Map.empty[String, List[Recipe]]
      // build all recipes
      val allRecipes = ingredients.zipWithIndex.map {
        case (list, index) =>
          val r = Recipe(name = recipes(index), requiredIngredientsCount = list.size)
          // build ingredients to recipes map
          list.foreach(i =>
            ingredientsToRecipes(i) = ingredientsToRecipes.get(i) match {
              case Some(s) => r :: s
              case None => List(r)
            }
          )
          r
      }
      // traverse supply chain
      val supplyQueue = mutable.Queue.from(supplies)
      while (supplyQueue.nonEmpty) {
        ingredientsToRecipes.get(supplyQueue.dequeue()) match {
          case Some(recipes) =>
            recipes.filter(!_.canBeMade).foreach { r =>
              r.addIngredient()
              // resupply newly produced components
              if (r.canBeMade) {
                supplyQueue.enqueue(r.name)
              }
            }
          case None =>
        }
      }
      allRecipes.filter(_.canBeMade).map(_.name)
    }

    case class Recipe(name: String, requiredIngredientsCount: Int, var existingIngredientsCount: Int = 0) {
      def addIngredient(): Unit = existingIngredientsCount = existingIngredientsCount + 1

      def canBeMade: Boolean = requiredIngredientsCount == existingIngredientsCount
    }
  }

  //println(Solution.findAllRecipes(recipes = Array("bread"),ingredients = List(List("yeast","flour")),supplies = Array("yeast","flour","corn")))
  //println(Solution.findAllRecipes(recipes = Array("bread","sandwich"),ingredients = List(List("yeast","flour"), List("bread","meat")),supplies = Array("yeast","flour","meat")))
  println(Solution.findAllRecipes(recipes = Array("bread", "sandwich", "burger"), ingredients = List(List("yeast", "flour"), List("bread", "meat"), List("sandwich", "meat", "bread")), supplies = Array("yeast", "flour", "meat")))

}
