/*
  Archivo:  package.scala
  Autores:  Julian Ernesto Puyo Mora <julian.puyo@correounivalle.edu.co> <202226905>
            Manuel Felipe Cardoso Forero <manuel.cardoso@correounivalle.edu.co> <2027288>
            Franklin Aguirre Ortiz <franklin.aguirre@correounivalle.edu.co> <1841743>
            Jean Paul Davalos Valencia <jean.davalos@correounivalle.edu.co> <1832375>
  Curso:    Fundamentos de Programación Funcional y Concurrente
  Trabajo:  Proyecto Final
  Fecha de entrega: 07/12/2023
*/

package object ArbolSufijos {
  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  abstract class Trie
  case class Nodo(car:Char, marcada:Boolean,
                   hijos: List[Trie]) extends Trie
  case class Hoja(car:Char, marcada:Boolean) extends Trie
  def raiz(t:Trie): Char = {
    t match {
      case Nodo(c,_,_) => c
      case Hoja(c,_) => c
    }
  }
  def cabezas(t:Trie): Seq[Char] = {
    t match {
      case Nodo(_,_,lt) => lt.map(t => raiz(t))
      case Hoja(c,_) => Seq[Char](c)
    }
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    // Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
    s match {
      case caracter::cola =>
          t match {
            case Nodo(_, _, hijos) => {
              val child = hijos.filter(hijo => raiz(hijo) == caracter)
              if (child.nonEmpty)
                pertenece(cola, child.head)
              else
                false
            }
            case Hoja(_, _) => false
          }
      case Nil =>
        t match {
          case Nodo(_, marcada, _) => marcada
          case Hoja(_, marcada) => marcada
        }
    }
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    // Prepara la "rama" a ser agregada al arbol correspondiente a la secuencia o resto de secuencia a ser añadida.
    def crearRama(s: Seq[Char]): Trie = {
      s match {
        case cabeza :: cola => cola match {
          case head :: tail => Nodo(cabeza, marcada = false, List(crearRama(cola)))
          case Nil => Hoja(cabeza, marcada = true)
        }
        case Nil => Nodo(' ', marcada = false, List())
      }
    }
    def agregarRama(arbolActual: Trie, prefix: Seq[Char], remaining: Seq[Char]): Trie = {
      (arbolActual, prefix, remaining) match {
        case (Nodo(car, marcada, hijos), _, head :: tail) if cabezas(Nodo(car, marcada, hijos)).contains(head) =>
          // Recorre recursivamente el árbol hasta llegar al camino deseado
          val updatedHijos = hijos.map { hijo =>
            if (raiz(hijo) == head) agregarRama(hijo, prefix :+ head, tail)
            else hijo
          }
          Nodo(car, marcada, updatedHijos)
        case (Hoja(car, marcada), _, head :: tail) =>
          // Convierte la hoja en un Nodo con el nuevo "subárbol" como hijo
          Nodo(car, marcada, List(crearRama(remaining)))
        case (Nodo(car, marcada, hijos), _, head :: tail) =>
          // Agrega el nuevo nodo a la lista de hijos cuando el camino se detiene en un Nodo
          Nodo(car, marcada, hijos :+ crearRama(remaining))
        case (Nodo(car, false, hijos), _, Nil) =>
          // Modifica el valor de marcada a true si no hay camino por recorrer pero los elementos de la cadena están en el arbol.
          Nodo(car, marcada = true, hijos)
        case (_, _, _) =>
          arbolActual
      }
    }
    agregarRama(t, Seq.empty[Char], s)
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // dada una secuencia no vacia de secuencias de vuelve el arbol de sufijos asociado a esas secuencias
    val arbolVacio: Trie = Nodo(' ', marcada = false, List())
    ss.foldLeft(arbolVacio) { (acc, s) => adicionar(s, acc) }
  }
}
