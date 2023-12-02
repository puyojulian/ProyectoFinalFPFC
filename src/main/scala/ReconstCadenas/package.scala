import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
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
  def reconstruirCadenaMejorado(n: Int, o: Oraculo ): Seq[Char] = {
    // recibela longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    val alfabeto = Seq('a', 'c', 'g', 't')
    def generarCadena(k: Int, SC:Set[Seq[Char]]): Seq[Char] = {
      val conjSec = SC.flatMap(seq => alfabeto.map(char => seq :+ char))
      val newSC = for {
        w <- conjSec
        if o(w)
      } yield {
        w
      }
      val resultado = newSC.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadena(k + 1, newSC)
      }
    }
    generarCadena(1, Set(Seq.empty[Char]))
  }

  def reconstruirCadenaTurbo(n : Int , o : Oraculo ): Seq [Char] = {
    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      val newSC = conjSec.filter(o)
      val resultado = newSC.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k*2, newSC)
      }
    }
    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurbo(2, conjuntoInicial)
  }
  def reconstruirCadenaTurboMejorada (n : Int , o : Oraculo ) : Seq [Char]= {
    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido

    // Definición del alfabeto
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val newSC = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter(s => o(s))
      newSC.find(_.length == n).getOrElse(Seq.empty[Char]) match {
        case seq if seq.nonEmpty => seq
        case _ => if (k > n) Seq.empty[Char] else generarCadenaTurbo(k * 2, newSC)
      }
    }

    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      SC.filterNot { s1 =>
        SC.exists(s2 => s1 != s2 && !o(s1 ++ s2 ++ s2 ++ s1))
      }
    }

    generarCadenaTurbo(2, alfabeto.map(Seq(_)).toSet)
  }
//  def reconstruirCadenaTurboAcelerada (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n , potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//    // Usa el filtro para ir mas rapido
//    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
//
//  }
}
