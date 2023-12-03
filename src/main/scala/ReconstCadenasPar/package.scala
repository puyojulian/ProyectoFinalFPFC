import common._

import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._

import scala.annotation.meta.param
import scala.collection.immutable.Set


package object ReconstCadenasPar {

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa paralelismo de tareas


    def reconstruirRec(seq: Seq[Char]): Seq[Char] = {
      if (seq.length == n) {
        if (o(seq)) seq
        else Seq.empty[Char]
      } else {
        val subproblems = alfabeto.flatMap(char => Seq(seq :+ char))

        val results = subproblems.par.map(subproblem => reconstruirRec(subproblem))

        if (subproblems.length <= umbral) {
          // Resolver de manera secuencial si el tamaño es menor o igual al umbral
          results.headOption.getOrElse(Seq.empty[Char])
        } else {
          // Resolver de manera paralela si el tamaño supera el umbral
          results.reduceOption((acc, curr) => if (o(acc) && acc.length >= curr.length) acc else curr)
            .getOrElse(Seq.empty[Char])
        }
      }
    }

    reconstruirRec(Seq.empty[Char])
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    //    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    //    // y devuelve la secuencia reconstruida
    //    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    //    // Usa paralelismo de tareas y/o datos

    def generarCadenaPar(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val newSC = if (SC.length <= umbral) {
        SC.flatMap(seq => alfabeto.view.map(char => seq :+ char).filter(o))
      } else {
        SC.par.flatMap(seq => alfabeto.view.map(char => seq :+ char).filter(o)).seq
      }

      newSC.find(w => w.length == n) match {
        case Some(seq) => seq
        case None =>
          if (k > n || newSC.isEmpty) Seq.empty[Char]
          else generarCadenaPar(k + 1, newSC)
      }
    }

    // Se comienza la recursión de cola con 'k = 1' y 'SC = Seq(Seq.empty[Char])'.
    generarCadenaPar(1, Seq(Seq.empty[Char]))

  }

//    def reconstruirCadenaMejoradoPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
//      // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
//      // y devuelve la secuencia reconstruida
//      // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//      // Usa paralelismo de tareas y/o datos
//
//    }
    def reconstruirCadenaTurboPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
      // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
      // y devuelve la secuencia reconstruida
      // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
      // Usa paralelismo de tareas y/o datos

      def generarCadena(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
        val conjSec = SC.flatMap(seq => alfabeto.map(char => seq :+ char))
        val newSC = if (conjSec.size <= umbral) {
          conjSec.filter(o)
        }
        else {
          val (left, right) = conjSec.par.splitAt(conjSec.size / 2)
          val ((l1cS, l2cS), (r1cS, r2cS)) = (left.splitAt(left.size / 2), right.splitAt(right.size / 2))
          val ((fl1, fl2), (fr1, fr2)) = (
            parallel(l1cS.filter(o), l2cS.filter(o)),
            parallel(r1cS.filter(o), r2cS.filter(o))
          )
          (fl1 ++ fl2 ++ fr1 ++ fr2).seq
        }
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
    def reconstruirCadenaTurboMejoradaPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
      // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
      // y devuelve la secuencia reconstruida
      // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
      // Usa paralelismo de tareas y/o datos

      // Función principal para generar la cadena turbo mejorada paralela
      def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
        val conjSec = filtrar(SC, k)
        val newSC = if (conjSec.size <= umbral) {
          conjSec.filter(o)
        } else {
          val (left, right) = conjSec.par.splitAt(conjSec.size / 2)
          val ((l1cS, l2cS), (r1cS, r2cS)) = (left.splitAt(left.size / 2), right.splitAt(right.size / 2))
          val ((fl1, fl2), (fr1, fr2)) = (
            parallel(l1cS.filter(o), l2cS.filter(o)),
            parallel(r1cS.filter(o), r2cS.filter(o))
          )
          (fl1 ++ fl2 ++ fr1 ++ fr2).seq
        }
        val resultado = newSC.to(LazyList).filter(w => w.length == n)
        if (resultado.nonEmpty) {
          resultado.head
        } else if (k > n) {
          Seq.empty[Char]
        } else {
          generarCadenaTurbo(k * 2, newSC)
        }
      }
      def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
        if (SC.size <= umbral) {
          SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter {
            s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k)))
          }
        }
        else {
          val S = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).par
          val (leftS, rightS) = S.splitAt(S.size / 2)
          val ((l1S, l2S), (r1S, r2S)) = (leftS.splitAt(S.size / 2), rightS.splitAt(S.size / 2))
          val ((l1F, l2F), (r1F, r2F)) = (
            parallel(l1S.filter { s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k))) },
              l2S.filter { s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k))) }),
            parallel(r1S.filter { s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k))) },
              r2S.filter { s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k))) })
          )
          val F = l1F ++ l2F ++ r1F ++ r2F
          F.seq
        }
      }
      val conjuntoInicial = alfabeto.map(Seq(_))
      generarCadenaTurbo(1, conjuntoInicial)
    }
    def reconstruirCadenaTurboAceleradaPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
      // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
      // y devuelve la secuencia reconstruida
      // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
      // Usa arboles de sufijos para guardar Seq[Seq[Char]]
      // Usa paralelismo de tareas y/o datos

      // Función principal para generar la cadena turbo mejorada paralela
      def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
        val conjSec = filtrar(SC, k)
        val newSC = if (conjSec.size <= umbral) {
          conjSec.filter(o)
        } else {
          val (left, right) = conjSec.par.splitAt(conjSec.size / 2)
          val ((l1cS, l2cS), (r1cS, r2cS)) = (left.splitAt(left.size / 2), right.splitAt(right.size / 2))
          val ((fl1, fl2), (fr1, fr2)) = (
            parallel(l1cS.filter(o), l2cS.filter(o)),
            parallel(r1cS.filter(o), r2cS.filter(o))
          )
          (fl1 ++ fl2 ++ fr1 ++ fr2).seq
        }
        val resultado = newSC.to(LazyList).filter(w => w.length == n)
        if (resultado.nonEmpty) {
          resultado.head
        } else if (k > n) {
          Seq.empty[Char]
        } else {
          generarCadenaTurbo(k * 2, newSC)
        }
      }

      def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
        val trieSC = arbolDeSufijos(SC)
        if (SC.size <= umbral) {
          SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter {
            s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k)))
          }
        }
        else {
          val S = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).par
          val (leftS, rightS) = S.splitAt(S.size / 2)
          val ((l1S, l2S), (r1S, r2S)) = (leftS.splitAt(S.size / 2), rightS.splitAt(S.size / 2))
          val ((l1F, l2F), (r1F, r2F)) = (
            parallel(l1S.filter { s => (0 to s.length - k).forall(i => pertenece(s.slice(i, i + k), trieSC)) },
              l2S.filter { s => (0 to s.length - k).forall(i => pertenece(s.slice(i, i + k), trieSC)) }),
            parallel(r1S.filter { s => (0 to s.length - k).forall(i => pertenece(s.slice(i, i + k), trieSC)) },
              r2S.filter { s => (0 to s.length - k).forall(i => pertenece(s.slice(i, i + k), trieSC)) })
          )
          val F = l1F ++ l2F ++ r1F ++ r2F
          F.seq
        }
      }

      val conjuntoInicial = alfabeto.map(Seq(_))
      generarCadenaTurbo(1, conjuntoInicial)
    }

}
