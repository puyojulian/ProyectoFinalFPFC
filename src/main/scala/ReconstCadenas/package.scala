import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  def reconstruirCadenaIngenuo (n : Int , o : Oraculo ) : Seq[Char]= {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarSecuencias(n: Int, alphabet: Seq[Char]): Set[Seq[Char]] = {
      val initialSet: Set[Seq[Char]] = Set(Seq.empty[Char])
      (1 to n).foldLeft(initialSet) { (acc, _) =>
        acc.flatMap(seq => alphabet.map(char => seq :+ char))
      }
    }

    val secuencias = generarSecuencias(n, alfabeto)

    val resultado = secuencias.to(LazyList).find(o)
    resultado.getOrElse(Seq.empty[Char])
  }
//  def reconstruirCadenaMejorado (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//
//  }
//  def reconstruirCadenaTurbo (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//
//  }
//  def reconstruirCadenaTurboMejorada (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//    // Usa el filtro para ir mas rapido
//
//  }
//  def reconstruirCadenaTurboAcelerada (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n , potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//    // Usa el filtro para ir mas rapido
//    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
//
//  }
}
