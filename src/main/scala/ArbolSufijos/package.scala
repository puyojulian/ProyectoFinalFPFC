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
                    val childOption: Option[Trie] = hijos.find(hijo => raiz(hijo) == caracter)
                    childOption match {
                      case Some(child) => perteneceInterna(cola, child)
                      case None => false
                    }
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
      false // If the input sequence is empty, consider it as not belonging to any tree
    else {
      t match {
        case Nodo(' ', _, hijos) => {
          val childOption: Option[Trie] = hijos.find(hijo => raiz(hijo) == s.head)
          childOption match {
            case Some(child) => perteneceInterna(s, child)
            case None => false
          }
        }
        case Hoja(_, _) => false
      }
    }
  }

  def estaEnArbol(s: Seq[Char], t: Trie): Boolean = {
    // Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
    def perteneceInterna(s: Seq[Char], t: Trie): Boolean = {
      s match {
        case head :: cola => cola match {
          case caracter :: tail => {
            t match {
              case Nodo(_, _, hijos) => {
                val childOption: Option[Trie] = hijos.find(hijo => raiz(hijo) == caracter)
                childOption match {
                  case Some(child) => perteneceInterna(cola, child)
                  case None => false
                }
              }
              case Hoja(_, _) => false
            }
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
      false // If the input sequence is empty, consider it as not belonging to any tree
    else {
      t match {
        case Nodo(' ', _, hijos) => {
          val childOption: Option[Trie] = hijos.find(hijo => raiz(hijo) == s.head)
          childOption match {
            case Some(child) => perteneceInterna(s, child)
            case None => false
          }
        }
        case Hoja(_, _) => false
      }
    }
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    // Adiciona una secuencia de uno o mas caracteres a un trie
    def crearRama(s: Seq[Char]): Trie = {
      s match {
        case cabeza :: cola => cola match {
          case head :: tail => Nodo(cabeza, false, List(crearRama(cola)))
          case Nil => Hoja(cabeza, true)
        }
        case Nil => Nodo(' ', false, List())
      }
    }

    def addToTrie(arbolOriginal: Trie, camino: Seq[Char], nuevaRama: Trie): Trie = {
      def agregarRama(arbolActual: Trie, caminoRestante: Seq[Char], nuevaRama: Trie): Trie = (arbolActual, caminoRestante, nuevaRama) match {
        case (Nodo(car, marcada, hijos), head :: tail, _) =>
          // Recursively traverse the tree until reaching the desired path
          val updatedHijos = hijos.map { hijo =>
            if (raiz(hijo) == head) agregarRama(hijo, tail, nuevaRama)
            else hijo
          }
          Nodo(car, marcada, updatedHijos)
        case (Hoja(car, marcada), Nil, _) =>
          // Convert the leaf into a Nodo with the new subtree as a child
          Nodo(car, marcada, List(nuevaRama))
        case (Nodo(car, marcada, hijos), Nil, _) =>
          // Add the new node to the list of children when the path stops at a Nodo
          Nodo(car, marcada, hijos :+ nuevaRama)
        case (Nodo(car, _, hijos), Nil, Nodo(' ', false, List())) =>
          // Cambia el valor de 'marcada' si termina el camino y no hay que agregar una nueva rama
          Nodo(car, marcada = true, hijos)
        case (_, Seq(), _) =>
          // Handle the case where the path is empty
          arbolActual
        case _ =>
          // For other cases, return the tree unchanged
          arbolActual
      }

      agregarRama(arbolOriginal, camino, nuevaRama)
    }

    def dividirSecuencia(s: Seq[Char], t: Trie): (Seq[Char], Seq[Char]) = {
      // Retorna el prefijo reconocido más largo
      val parteReconocida = s.inits.find(prefix => estaEnArbol(prefix, t)).getOrElse(Seq.empty)
      // La parte no reconocida es la diferencia entre la secuencia original y la parte reconocida.
      val parteNoReconocida = s.drop(parteReconocida.length)
      (parteReconocida, parteNoReconocida)
    }

    val (secuenciaEnArbol, secuenciaNoEnArbol) = dividirSecuencia(s, t)
    val nuevaRama = crearRama(secuenciaNoEnArbol)

    addToTrie(t, secuenciaEnArbol, nuevaRama)
  }


  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // dada una secuencia no vacia de secuencias de vuelve el arbol de sufijos asociado a esas secuencias
    val arbolVacío: Trie = Nodo(' ', false, List())
    ss.foldLeft(arbolVacío) { (acc, s) =>
      adicionar(s, acc)
    }
  }
}
