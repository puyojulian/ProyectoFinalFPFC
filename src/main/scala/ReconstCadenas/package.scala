import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida.
    // Genera todas las secuencias de caracteres de tamaño 'n' basadas en 'alfabeto'
    // y retorna la primera que cumple 'o'.
    (1 to n).foldLeft(Seq(Seq.empty[Char])) { (acc, _) =>
      acc.flatMap(seq => alfabeto.map(char => seq :+ char))
    }.to(LazyList).filter(o).head
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

    // Concatena a la derecha, en cada llamado recursivo, los caracteres del 'alfabeto' con
    // cada secuencia dentro del conjunto de secuencias conseguido en la "iteración" inmediatamente anterior.
    // Para posteriormente ser filtrado dentro de la expresión for para que solo queden aquellas 'w' que cumplan 'o(w)' ('newSC').
    // Finalmente, se retorna el resultado de buscar la secuencia 'w' que cumpla 'w.length == n' en 'newSC'.
    // Donde si no se encuentra la secuencia buscada en 'n' recursiones se retorna una secuencia vacía
    // (Se debería encontrar dentro de dicha cantidad de "iteraciones").
    def generarCadena(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val newSC = SC.flatMap(seq => alfabeto.map(char => seq :+ char)).filter(o)
      val resultado = newSC.to(LazyList).filter(w => w.length == n)
      if (resultado.nonEmpty) {
        resultado.head
      } else if (k > n) {
        Seq.empty[Char]
      } else {
        generarCadena(k + 1, newSC)
      }
    }
    // Se comienza la recursión de cola con 'k = 1' y 'SC = Seq(Seq.empty[Char])'.
    generarCadena(1, Seq(Seq.empty[Char]))
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq [Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

    // En base al algoritmo anterior, se mejora la implementación, cambiando en que ya no se
    // concatena el alfabeto sino las secuencias obtenidas en la "iteración" inmediatamente anterior.
    // cadenas del doble de tamaño. Así mismo, son filtradas y sometidas a la misma evaluación hasta encontrar la
    // secuencia de caracteres buscada. El avance de k será de 'k*2' en vez de 'k+1'.
    def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val newSC = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter(o)
      val resultado = newSC.to(LazyList).filter(w => w.length == n)
      if (resultado.nonEmpty) {
        resultado.head
      } else if (k > n) {
        Seq.empty[Char]
      } else {
        generarCadenaTurbo(k * 2, newSC)
      }
    }
    // Conjunto inicial de secuencias de longitud 1 del alfabeto, nótese la conversión necesaria.
    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    // Se comienza la recursión de cola con 'k = 1' y 'SC = conjuntoInicial'.
    generarCadenaTurbo(1, conjuntoInicial)
  }
  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo) : Seq [Char]= {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido

    // La implementación anterior se mejora agregando 'filtrar()', la cual genera 'conjSec' pero a la vez
    // conservando solo aquellas secuencias cuyas "componentes" de tamaño 'k' pertenecen a 'SC'.
    def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val newSC = filtrar(SC, k).filter(o)
      val resultado = newSC.to(LazyList).filter(w => w.length == n)
      if (resultado.nonEmpty) {
        resultado.head
      } else if (k > n) {
        Seq.empty[Char]
      } else {
        generarCadenaTurbo(k * 2, newSC)
      }
    }
    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada
    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter {
        s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k)))
      }
    }
    // Conjunto inicial de secuencias de longitud 1 del alfabeto, nótese la conversión necesaria.
    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    // Se comienza la recursión de cola con 'k = 1' y 'SC = conjuntoInicial'.
    generarCadenaTurbo(1, conjuntoInicial)
  }
  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo) : Seq[Char]= {
    // recibe la longitud de la secuencia que hay que reconstruir (n , potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]

    // La implementación anterior se mejora utilizando una estructura de arbol de sufijos para
    // disminuir el tiempo en que se puede confirmar la pertenencia o no de una cadena dentro
    // del "conjunto" de secuencias actual ('SC'), específicamente dentro de 'filtrar()' ('trieSC').
    def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = conjSec.filter(o)
      val resultado = newSC.to(LazyList).filter(w => w.length == n)
      if (resultado.nonEmpty) {
        resultado.head
      } else if (k > n) {
        Seq.empty[Char]
      } else {
        generarCadenaTurbo(k * 2, newSC)
      }
    }
    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada.
    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val trieSC = arbolDeSufijos(SC)
      SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter {
        s => (0 to s.length - k).forall(i => pertenece(s.slice(i, i + k), trieSC))
      }
    }
    // Conjunto inicial de secuencias de longitud 1 del alfabeto, nótese la conversión necesitada.
    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    // Se comienza la recursión de cola con 'k = 1' y 'SC = conjuntoInicial'.
    generarCadenaTurbo(1, conjuntoInicial)
  }
}
