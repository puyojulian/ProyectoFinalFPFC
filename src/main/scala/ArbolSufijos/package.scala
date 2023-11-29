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
    def perteneceInterna(s: Seq[Char], t: Trie): Boolean = {
      s match {
        case head::cola => cola match {
          case caracter::tail => {
            t match {
              case Nodo(_, _, hijos) => {
                val child = hijos.filter(hijo => raiz(hijo) == caracter)
                if (child.nonEmpty)
                  perteneceInterna(cola, child.head)
                else
                  false
              }
              case Hoja(_, _) => false
            }
          }
          case Nil =>
            t match {
              case Nodo(_, marcada, _) => marcada
              case Hoja(_, marcada) => marcada
            }
        }
        case Nil =>
          t match {
            case Nodo(_, marcada, _) => marcada
            case Hoja(_, marcada) => marcada
          }
      }
    }
    if (s.isEmpty)
      false // Si la secuencia de entrada esta vacia, se considera que no pertenece a ningun arbol.
    else {
      t match {
        case Nodo(' ', _, hijos) => {
          val child = hijos.filter(hijo => raiz(hijo) == s.head)
          if (child.nonEmpty)
            perteneceInterna(s, child.head)
          else
            false
        }
        case Hoja(_, _) => false
      }
    }
  }

  def perteneceLaxa(s: Seq[Char], t: Trie): Boolean = {
    def perteneceLaxaInterna(s: Seq[Char], t: Trie): Boolean = {
      s match {
        case head :: cola => cola match {
          case caracter :: tail =>
            t match {
              case Nodo(_, _, hijos) =>
                val child = hijos.filter(hijo => raiz(hijo) == caracter)
                if (child.nonEmpty)
                  perteneceLaxaInterna(cola, child.head)
                else
                  false
              case Hoja(_, _) => false
            }
          case Nil =>
            t match {
              case Nodo(_, marcada, _) => true
              case Hoja(_, marcada) => true
            }
        }
        case Nil =>
          t match {
            case Nodo(_, marcada, _) => true
            case Hoja(_, marcada) => true
          }
      }
    }
    if (s.isEmpty)
      false
    else {
      t match {
        case Nodo(' ', _, hijos) =>
          val child = hijos.filter(hijo => raiz(hijo) == s.head)
          if (child.nonEmpty)
            perteneceLaxaInterna(s, child.head)
          else
            false
        case Hoja(_, _) => false
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
    def agregarRama(arbolActual: Trie, caminoRestante: Seq[Char], nuevaRama: Trie): Trie = {
      (arbolActual, caminoRestante) match {
        case (Nodo(car, marcada, hijos), head :: tail) =>
          // Recorre recursivamente el árbol hasta llegar al camino deseado
          val updatedHijos = hijos.map { hijo =>
            if (raiz(hijo) == head) agregarRama(hijo, tail, nuevaRama)
            else hijo
          }
          Nodo(car, marcada, updatedHijos)
        case (Hoja(car, marcada), Nil) =>
          // Convierte la hoja en un Nodo con el nuevo "subárbol" como hijo
          Nodo(car, marcada, List(nuevaRama))
        case (Nodo(car, marcada, hijos), Nil) =>
          // Agrega el nuevo nodo a la lista de hijos cuando el camino se detiene en un Nodo
          Nodo(car, marcada, hijos :+ nuevaRama)
      }
    }
    // Divide la secuencia en dos: Una que se encuentra dentro del arbol y otra que no está para ser agregada.
    def dividirSecuencia(s: Seq[Char], t: Trie): (Seq[Char], Seq[Char]) = {
      // Retorna el prefijo reconocido más largo
      val parteReconocida = s.inits.find(prefix => perteneceLaxa(prefix, t)).getOrElse(Seq.empty)
      // La parte no reconocida es la diferencia entre la secuencia original y la parte reconocida.
      val parteNoReconocida = s.drop(parteReconocida.length)
      (parteReconocida, parteNoReconocida)
    }
    val (secuenciaEnArbol, secuenciaNoEnArbol) = dividirSecuencia(s, t)
    val nuevaRama = crearRama(secuenciaNoEnArbol)
    agregarRama(t, secuenciaEnArbol, nuevaRama)
  }


  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // dada una secuencia no vacia de secuencias de vuelve el arbol de sufijos asociado a esas secuencias
    val arbolVacio: Trie = Nodo(' ', marcada = false, List())
    ss.foldLeft(arbolVacio) { (acc, s) => adicionar(s, acc) }
  }
}
