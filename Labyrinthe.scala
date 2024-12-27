import scala.{Array => $}

/**
 * Objet principal pour exécuter le programme du labyrinthe.
 */
object Main {

  /**
   * Point d'entrée principal du programme.
   * @param args Arguments de ligne de commande.
   */
  def main(args: Array[String]): Unit = {
    val laby = new Labyrinthe($(
      $(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      $(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1),
      $(1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1),
      $(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
      $(1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1),
      $(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1),
      $(1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
      $(1, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    ))
    println(laby)
    println

    laby.cheminSortieAPartirDe((0, 1)).foreach(s => print((new Labyrinthe(s)).toString + "\n\n"))
  }
}

/**
 * Classe représentant un labyrinthe.
 * @param init_ Tableau bidimensionnel représentant la configuration initiale du labyrinthe.
 */
class Labyrinthe(init_ : Array[Array[Int]]) {

  /**
   * Trouve tous les chemins menant à la sortie à partir d'une position donnée.
   * @param pos_ Coordonnées initiales sous la forme d'un tuple (x, y).
   * @param lab_ Tableau représentant l'état actuel du labyrinthe (par défaut, utilise le labyrinthe initial).
   * @return Un ensemble de tableaux représentant tous les chemins vers la sortie.
   */
  def cheminSortieAPartirDe(pos_ : (Int, Int), lab_ : Array[Array[Int]] = init_): Set[Array[Array[Int]]] = {
    pos_ match {
      case (x, y) if lab_ (x)(y) == 9 =>
        Set(lab_.map(_.clone()))
      case (x, y) if lab_ (x)(y) == 0 =>
        List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
          .filter {
            case (nx, ny) => nx >= 0 && ny >= 0 && nx < lab_ .length && ny < lab_(0).length && lab_ (nx)(ny) != 1 && lab_ (nx)(ny) != 2
          }
          .flatMap {
            case neighbor => cheminSortieAPartirDe(neighbor, lab_ .updated(x, lab_ (x).updated(y, 2)))
          }
          .toSet
      case _ =>
        Set.empty[Array[Array[Int]]]
    }
  }

  /**
   * Retourne une représentation textuelle du labyrinthe, avec des couleurs.
   * Les valeurs spécifiques dans le labyrinthe sont affichées comme suit :
   * - '1' : Rouge (mur).
   * - '0' : Blanc (chemin libre).
   * - '2' : Bleu (chemin exploré).
   * - '9' : Vert (sortie).
   * @return Une chaîne représentant le labyrinthe.
   */
  override def toString: String = {
    init_ .map(_ .map {
      case 1 => "\u001B[41m 1 \u001B[0m"
      case 0 => " 0 "
      case 2 => "\u001B[44m 2 \u001B[0m"
      case 9 => "\u001B[42m 9 \u001B[0m"
    }.mkString("")).mkString("\n")
  }
}
