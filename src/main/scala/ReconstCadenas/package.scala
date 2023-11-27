import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    val alfabeto = Seq('a', 'c', 'g', 't')
    // genera todas las secuencias de caracteres de tamaño 'size' basadas en el alfabeto 'alphabet'.
    def generarSecuencias(size: Int, alphabet: Seq[Char]): Set[Seq[Char]] = {
      val initialSet: Set[Seq[Char]] = Set(Seq.empty[Char])
      (1 to size).foldLeft(initialSet) { (acc, _) =>
        acc.flatMap(seq => alphabet.map(char => seq :+ char))
      }
    }
    // Almacena el valor de generarSecuencias(size = n, alphabet = alfabeto) en secuencias.
    val secuencias = generarSecuencias(n, alfabeto)
    // Busca en 'secuencias' aquella que cumpla 'o(secuencia)'. Se usa 'to(LazyList)' para que 'find()'
    // deje de buscar si se ha encontrado.
    val resultado = secuencias.to(LazyList).find(o)
    // Se retorna el valor encontrado por 'find(o)' ('resultado = Some(secuencia)'),
    // o una secuencia vacía si no encontró nada ('resultado = None')
    resultado.getOrElse(Seq.empty[Char])
  }
  def reconstruirCadenaMejorado(n: Int, o: Oraculo ): Seq[Char] = {
    // recibela longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    val alfabeto = Seq('a', 'c', 'g', 't')
    // Concatena a la derecha, en cada llamado recursivo, los caracteres del 'alfabeto' con
    // cada secuencia dentro del conjunto de secuencias conseguido en la "iteración" inmediatamente anterior ('conjSec').
    // Para posteriormente ser filtrado dentro de la expresión for para que solo queden aquellas 'w' que cumplan 'o(w)' ('newSC').
    // Finalmente, se retorna el resultado de buscar la secuencia 'w' que cumpla 'w.length == n' en 'newSC'.
    // Donde si no se encuentra la secuencia buscada en 'n' recursiones se retorna una secuencia vacía
    // (Se debería encontrar dentro de dicha cantidad de "iteraciones").
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
        case Some(secuencia) => secuencia
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadena(k + 1, newSC)
      }
    }
    // Se comienza la recursión de cola con 'k = 1' y 'SC = Set(Seq.empty[Char])'.
    generarCadena(1, Set(Seq.empty[Char]))
  }

  def reconstruirCadenaTurbo(n : Int , o : Oraculo ): Seq [Char] = {
    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

    val alfabeto = Seq('a', 'c', 'g', 't')
    // En base al algoritmo anterior, se mejora la implementación, cambiando en que ya no se
    // concatena el alfabeto sino las secuencias obtenidas en la "iteración" inmediatamente anterior.
    // cadenas del doble te tamaño. Así mismo, son filtradas y sometidas a la misma evaluación hasta encontrar la
    // secuencia de caracteres buscada. El avance de k será de 'k*2' en vez de 'k+1'.
    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      val newSC = conjSec.filter(o)
      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(secuencia) => secuencia
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k*2, newSC)
      }
    }
    // Conjunto inicial de secuencias de longitud 1 del alfabeto, nótese la conversión necesaria.
    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    // Se comienza la recursión de cola con 'k = 1' y 'SC = conjuntoInicial'.
    generarCadenaTurbo(1, conjuntoInicial)
  }
  def reconstruirCadenaTurboMejorada (n : Int , o : Oraculo ) : Seq [Char]= {
    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido

    // Definición del alfabeto
    val alfabeto = Seq('a', 'c', 'g', 't')

    // La implementación anterior se mejora agregando 'filtrar()', la cual genera 'conjSec' pero a la vez
    // conservando solo aquellas secuencias cuyas "componentes" de tamaño 'k' pertenecen a 'SC'.
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
      val S = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      val F = S.filter { s => s.sliding(k).forall(w => SC(w)) }
      F
    }

    // Conjunto inicial de secuencias de longitud 1 del alfabeto, nótese la conversión necesaria.
    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet

    // Se comienza la recursión de cola con 'k = 1' y 'SC = conjuntoInicial'.
    generarCadenaTurbo(1, conjuntoInicial)
  }

  def reconstruirCadenaTurboAcelerada(n : Int , o : Oraculo ) : Seq[Char]= {
    // recibela longitud de la secuencia que hay que reconstruir (n , potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]

    // Definición del alfabeto
    val alfabeto = Seq('a', 'c', 'g', 't')

    // La implementación anterior se mejora utilizando una estructura de arbol de sufijos para
    // disminuir el tiempo en que se puede confirmar la pertenencia o no de una cadena dentro
    // del "conjunto" de secuencias actual ('SC'), específicamente dentro de 'filtrar()' ('trieSC').
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

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada.
    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      val trieSC = arbolDeSufijos(SC.toSeq)
      val S = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      val F = S.filter{s => s.sliding(k).forall(w => pertenece(w, trieSC))}
      F
    }

    // Conjunto inicial de secuencias de longitud 1 del alfabeto, nótese la conversión necesitada.
    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet

    // Se comienza la recursión de cola con 'k = 1' y 'SC = conjuntoInicial'.
    generarCadenaTurbo(1, conjuntoInicial)
  }
}
