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
//  def pertenece(s: Seq[Char], t: Trie): Boolean = {
//    // Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
//    val heads = cabezas(t)
//    val continuara = heads.to(LazyList).find(_ == s.head)
//    continuara match {
//      case Some(char) => {
//        s.tail match {
//          case caracter::cola => {
//            t match {
//              case Nodo(_, _, hijos) =>
//                val childOption: Option[Trie] = hijos.find { hijo =>
//                  cabezas(hijo).to(LazyList).find(_ == caracter).contains(raiz(hijo))
//                }
//                childOption match {
//                  case Some(child) => pertenece(cola, child)
//                  case None => false
//                }
//              case Hoja(_, _) => false
//            }
//          }
//          case Nil => {
//            t match {
//              case Nodo(_, _, _) => false
//              case Hoja(_, marcada) => if (marcada) true else false
//            }
//          }
//        }
//      }
//      case None => false
//    }
//  }

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

//  def adicionar(s: Seq[Char], t: Trie): Trie = {
//    // Adiciona una secuencia de uno o mas caracteres a un trie
//
//  }
//  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
//    // dada una secuencia no vacia de secuencias de vuelve el arbol de sufijos asociado a esas secuencias
//
//  }
}
