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
      val resultado = newSC.to(LazyList).find(w => w.length == n)
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
      val resultado = newSC.to(LazyList).find(w => w.length == n)
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

    // Función principal para generar la cadena turbo mejorada
    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = conjSec.filter(o)

      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k * 2, newSC)
      }
    }

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada
    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      val S = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      val F = S.filter { s => s.sliding(k).forall(w => SC(w)) }
      F
    }

    // Conjunto inicial de secuencias de longitud 1 del alfabeto
    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet

    // Llamada inicial a la función de generación
    generarCadenaTurbo(1, conjuntoInicial)
  }

  def reconstruirCadenaTurboAcelerada(n : Int , o : Oraculo ) : Seq [Char]= {
    // recibela longitud de la secuencia que hay que reconstruir (n , potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]

    // Definición del alfabeto
    val alfabeto = Seq('a', 'c', 'g', 't')

    // Función principal para generar la cadena turbo mejorada
    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = conjSec.filter(o)

      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(secuencia) => secuencia
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k * 2, newSC)
      }
    }

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada
    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      val trieSC = arbolDeSufijos(SC.toSeq)
      val S = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      val F = S.filter{s => s.sliding(k).forall(w => pertenece(w, trieSC))}
      F
    }

    // Conjunto inicial de secuencias de longitud 1 del alfabeto
    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet

    // Llamada inicial a la función de generación
    generarCadenaTurbo(1, conjuntoInicial)
  }
}
