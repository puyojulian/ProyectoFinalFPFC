/*
  Archivo:  Pruebas.sc
  Autores:  Julian Ernesto Puyo Mora <julian.puyo@correounivalle.edu.co> <202226905>
            Manuel Felipe Cardoso Forero <manuel.cardoso@correounivalle.edu.co> <2027288>
            Franklin Aguirre Ortiz <franklin.aguirre@correounivalle.edu.co> <1841743>
            Jean Paul Davalos Valencia <jean.davalos@correounivalle.edu.co> <1832375>
  Curso:    Fundamentos de Programación Funcional y Concurrente
  Trabajo:  Proyecto Final
  Fecha de entrega: 07/12/2023
*/

import Oraculo._
import ReconstCadenas._
import scala.util.Random
import ReconstCadenasPar._
import ArbolSufijos._
import Benchmark._

val random = new Random()

def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
  //Crea una secuencia de long caracteres del alfabeto,
  // escogidos de forma aleatoria, terminando en s
  if (s.length==long) s
  else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long,alfabeto(indiceAzar)+:s)
  }
}

// PRUEBAS: ENUNCIADO

val costoOraculo = 1
//
//val sec1=Seq('a', 'c', 'c', 'a')
//val sec2 = Seq('a', 'c', 'g', 'c', 'a')
//val sec3=secAlAzar(10,Seq())
//
//val or_1=crearOraculo(costoOraculo)(sec1)
//val or_2=crearOraculo(costoOraculo)(sec2)
//val or_3=crearOraculo(costoOraculo)(sec3)
//
//reconstruirCadenaIngenuo(sec1.length, or_1)
//reconstruirCadenaIngenuo(sec2.length, or_2)
//reconstruirCadenaIngenuo(sec3.length, or_3)
//reconstruirCadenaMejorado(sec3.length, or_3)
//
//
def secsCortasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(i,Seq())
} yield s

def secsLargasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(math.pow(2,i).toInt,Seq())
} yield s


// Secuencias para pruebas
val ss1_10=secsCortasParaPruebas(10)
val ss1_16=secsCortasParaPruebas(16)
val ss2_1024 = secsLargasParaPruebas(10)
val ss2_2048 = secsLargasParaPruebas(11)
val ss2_4096 = secsLargasParaPruebas(12)
val s1_8 = ss1_10(7)
val s2_8 = ss1_16(7)
val s1_10 = ss1_10.reverse(0)
val s1_11=ss1_16(10)
val s1_12 = ss1_16(11)
val s1_13 = ss1_16(12)
val s1_14 = ss1_16(13)
val s1_15 = ss1_16(14)
val s1_16 = ss1_16(15)
val s1_32 = ss2_1024(4)
val s2_32 = ss2_2048(4)
val s3_32 = ss2_4096(4)
val s1_64 = ss2_1024(5)
val s2_64 = ss2_2048(5)
val s3_64 = ss2_4096(5)
val s1_128 = ss2_1024(6)
val s2_128 = ss2_2048(6)
val s3_128 = ss2_4096(6)
val s1_256 = ss2_1024(7)
val s2_256 = ss2_2048(7)
val s3_256 = ss2_4096(7)
val s1_512 = ss2_1024(8)
val s2_512 = ss2_2048(8)
val s3_512 = ss2_4096(8)
//val s1_1024 = ss2_1024(9)
//val s2_1024 = ss2_2048(9)
//val s3_1024 = ss2_4096(9)
//val s1_2048 = ss2_2048(10)
//val s2_2048 = ss2_4096(10)
//val s1_4096 = ss2_4096(11)

//def pruebasIngenuo(ss:Seq[Seq[Char]]) = for {
//  s <- ss
//  o = crearOraculo(costoOraculo)(s)
//} yield (s,reconstruirCadenaIngenuo(s.length,o))
//
//def pruebasMejorado (ss:Seq[Seq[Char]]) = for {
//  s <- ss
//  o = crearOraculo(costoOraculo)(s)
//} yield (s.length, s,reconstruirCadenaMejorado(s.length,o))
//
//def pruebasTurbo (ss:Seq[Seq[Char]]) = for {
//  s <- ss
//  o = crearOraculo(costoOraculo)(s)
//} yield (s.length, s,reconstruirCadenaTurbo(s.length,o))
//
//def pruebasTurboMejorada (ss:Seq[Seq[Char]]) = for {
//  s <- ss
//  o = crearOraculo(costoOraculo)(s)
//} yield (s.length, s,reconstruirCadenaTurboMejorada(s.length,o))
//
//def pruebasTurboAcelerada(ss:Seq[Seq[Char]]) = for {
//  s <- ss
//  o = crearOraculo(costoOraculo)(s)
//} yield (s.length, s,reconstruirCadenaTurboAcelerada(s.length,o))

//// Pruebas funcionales
//
//// secuencias de longitud 8
//reconstruirCadenaIngenuo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//reconstruirCadenaIngenuo(s2_8.length, crearOraculo(costoOraculo)(s2_8))
//reconstruirCadenaMejorado(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//reconstruirCadenaMejorado(s2_8.length, crearOraculo(costoOraculo)(s2_8))
//reconstruirCadenaTurbo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//reconstruirCadenaTurbo(s2_8.length, crearOraculo(costoOraculo)(s2_8))
//reconstruirCadenaTurboMejorada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//reconstruirCadenaTurboMejorada(s2_8.length, crearOraculo(costoOraculo)(s2_8))
//reconstruirCadenaTurboAcelerada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//reconstruirCadenaTurboAcelerada(s2_8.length, crearOraculo(costoOraculo)(s2_8))
//
////// secuencias de longitud 16
//reconstruirCadenaMejorado(s1_16.length, crearOraculo(costoOraculo)(s1_16))
//reconstruirCadenaTurbo(s1_16.length, crearOraculo(costoOraculo)(s1_16))
//reconstruirCadenaTurboMejorada(s1_16.length, crearOraculo(costoOraculo)(s1_16))
//reconstruirCadenaTurboAcelerada(s1_16.length, crearOraculo(costoOraculo)(s1_16))
//
//
//// secuencias de longitud 32
//reconstruirCadenaMejorado(s1_32.length, crearOraculo(costoOraculo)(s1_32))
//reconstruirCadenaMejorado(s2_32.length, crearOraculo(costoOraculo)(s2_32))
//reconstruirCadenaMejorado(s3_32.length, crearOraculo(costoOraculo)(s3_32))
//reconstruirCadenaTurbo(s1_32.length, crearOraculo(costoOraculo)(s1_32))
//reconstruirCadenaTurbo(s2_32.length, crearOraculo(costoOraculo)(s2_32))
//reconstruirCadenaTurbo(s3_32.length, crearOraculo(costoOraculo)(s3_32))
//reconstruirCadenaTurboMejorada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
//reconstruirCadenaTurboMejorada(s2_32.length, crearOraculo(costoOraculo)(s2_32))
//reconstruirCadenaTurboMejorada(s3_32.length, crearOraculo(costoOraculo)(s3_32))
//reconstruirCadenaTurboAcelerada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
//reconstruirCadenaTurboAcelerada(s2_32.length, crearOraculo(costoOraculo)(s2_32))
//reconstruirCadenaTurboAcelerada(s3_32.length, crearOraculo(costoOraculo)(s3_32))
//
//
//// secuencias de longitud 64
//reconstruirCadenaMejorado(s1_64.length, crearOraculo(costoOraculo)(s1_64))
//reconstruirCadenaMejorado(s2_64.length, crearOraculo(costoOraculo)(s2_64))
//reconstruirCadenaMejorado(s3_64.length, crearOraculo(costoOraculo)(s3_64))
//reconstruirCadenaTurbo(s1_64.length, crearOraculo(costoOraculo)(s1_64))
//reconstruirCadenaTurbo(s2_64.length, crearOraculo(costoOraculo)(s2_64))
//reconstruirCadenaTurbo(s3_64.length, crearOraculo(costoOraculo)(s3_64))
//reconstruirCadenaTurboMejorada(s1_64.length, crearOraculo(costoOraculo)(s1_64))
//reconstruirCadenaTurboMejorada(s2_64.length, crearOraculo(costoOraculo)(s2_64))
//reconstruirCadenaTurboMejorada(s3_64.length, crearOraculo(costoOraculo)(s3_64))
//reconstruirCadenaTurboAcelerada(s1_64.length, crearOraculo(costoOraculo)(s1_64))
//reconstruirCadenaTurboAcelerada(s2_64.length, crearOraculo(costoOraculo)(s2_64))
//reconstruirCadenaTurboAcelerada(s3_64.length, crearOraculo(costoOraculo)(s3_64))
//
//// secuencias de longitud 128
//reconstruirCadenaMejorado(s1_128.length, crearOraculo(costoOraculo)(s1_128))
//reconstruirCadenaMejorado(s2_128.length, crearOraculo(costoOraculo)(s2_128))
//reconstruirCadenaMejorado(s3_128.length, crearOraculo(costoOraculo)(s3_128))
//reconstruirCadenaTurbo(s1_128.length, crearOraculo(costoOraculo)(s1_128))
//reconstruirCadenaTurbo(s2_128.length, crearOraculo(costoOraculo)(s2_128))
//reconstruirCadenaTurbo(s3_128.length, crearOraculo(costoOraculo)(s3_128))
//reconstruirCadenaTurboMejorada(s1_128.length, crearOraculo(costoOraculo)(s1_128))
//reconstruirCadenaTurboMejorada(s2_128.length, crearOraculo(costoOraculo)(s2_128))
//reconstruirCadenaTurboMejorada(s3_128.length, crearOraculo(costoOraculo)(s3_128))
//reconstruirCadenaTurboAcelerada(s1_128.length, crearOraculo(costoOraculo)(s1_128))
//reconstruirCadenaTurboAcelerada(s2_128.length, crearOraculo(costoOraculo)(s2_128))
//reconstruirCadenaTurboAcelerada(s3_128.length, crearOraculo(costoOraculo)(s3_128))
//
//// secuencias de longitud 256
//reconstruirCadenaMejorado(s1_256.length, crearOraculo(costoOraculo)(s1_256))
//reconstruirCadenaMejorado(s2_256.length, crearOraculo(costoOraculo)(s2_256))
//reconstruirCadenaMejorado(s3_256.length, crearOraculo(costoOraculo)(s3_256))
//reconstruirCadenaTurbo(s1_256.length, crearOraculo(costoOraculo)(s1_256))
//reconstruirCadenaTurbo(s2_256.length, crearOraculo(costoOraculo)(s2_256))
//reconstruirCadenaTurbo(s3_256.length, crearOraculo(costoOraculo)(s3_256))
//reconstruirCadenaTurboMejorada(s1_256.length, crearOraculo(costoOraculo)(s1_256))
//reconstruirCadenaTurboMejorada(s2_256.length, crearOraculo(costoOraculo)(s2_256))
//reconstruirCadenaTurboMejorada(s3_256.length, crearOraculo(costoOraculo)(s3_256))
//reconstruirCadenaTurboAcelerada(s1_256.length, crearOraculo(costoOraculo)(s1_256))
//reconstruirCadenaTurboAcelerada(s2_256.length, crearOraculo(costoOraculo)(s2_256))
//reconstruirCadenaTurboAcelerada(s3_256.length, crearOraculo(costoOraculo)(s3_256))
//
//// secuencias de longitud 512
//reconstruirCadenaMejorado(s1_512.length, crearOraculo(costoOraculo)(s1_512))
//reconstruirCadenaMejorado(s2_512.length, crearOraculo(costoOraculo)(s2_512))
//reconstruirCadenaMejorado(s3_512.length, crearOraculo(costoOraculo)(s3_512))
//reconstruirCadenaTurbo(s1_512.length, crearOraculo(costoOraculo)(s1_512))
//reconstruirCadenaTurbo(s2_512.length, crearOraculo(costoOraculo)(s2_512))
//reconstruirCadenaTurbo(s3_512.length, crearOraculo(costoOraculo)(s3_512))
//reconstruirCadenaTurboMejorada(s1_512.length, crearOraculo(costoOraculo)(s1_512))
//reconstruirCadenaTurboMejorada(s2_512.length, crearOraculo(costoOraculo)(s2_512))
//reconstruirCadenaTurboMejorada(s3_512.length, crearOraculo(costoOraculo)(s3_512))
//reconstruirCadenaTurboAcelerada(s1_512.length, crearOraculo(costoOraculo)(s1_512))
//reconstruirCadenaTurboAcelerada(s2_512.length, crearOraculo(costoOraculo)(s2_512))
//reconstruirCadenaTurboAcelerada(s3_512.length, crearOraculo(costoOraculo)(s3_512))
//
//
//// secuencias de longitud 1024
//reconstruirCadenaMejorado(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
//reconstruirCadenaMejorado(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
//reconstruirCadenaMejorado(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))
//reconstruirCadenaTurbo(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
//reconstruirCadenaTurbo(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
//reconstruirCadenaTurbo(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))
//reconstruirCadenaTurboMejorada(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
//reconstruirCadenaTurboMejorada(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
//reconstruirCadenaTurboMejorada(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))
//reconstruirCadenaTurboAcelerada(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
//reconstruirCadenaTurboAcelerada(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
//reconstruirCadenaTurboAcelerada(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))

//// Pruebas por lotes
//
//pruebasIngenuo(ss1_10)
//pruebasIngenuo(ss1_16.slice(0,10))
//pruebasIngenuo(ss1_16.slice(0,11))
//pruebasIngenuo(ss1_16.slice(0,12))
//pruebasIngenuo(ss1_16.slice(0,13))
//pruebasIngenuo(ss1_16.slice(0,14))
//pruebasIngenuo(ss1_16.slice(0,15))
//pruebasIngenuo(ss1_16)
//
//// con n=13 casi no puede, con n= 14, no pudo
//
//pruebasMejorado(ss1_10)
//pruebasMejorado(ss1_16.slice(0,10))
//pruebasMejorado(ss1_16.slice(0,11))
//pruebasMejorado(ss1_16.slice(0,12))
//pruebasMejorado(ss1_16.slice(0,13))
//pruebasMejorado(ss1_16.slice(0,14))
//pruebasMejorado(ss1_16.slice(0,15))
//pruebasMejorado(ss1_16)
//pruebasMejorado(ss2_1024)
//pruebasMejorado(ss2_2048)
//pruebasMejorado(ss2_4096)
//// con n=2^11  pudo, con n= 2^12, no pudo
//
//pruebasTurbo(ss1_10)
//pruebasTurbo(ss1_16.slice(0,10))
//pruebasTurbo(ss1_16.slice(0,11))
//pruebasTurbo(ss1_16.slice(0,12))
//pruebasTurbo(ss1_16.slice(0,13))
//pruebasTurbo(ss1_16.slice(0,14))
//pruebasTurbo(ss1_16.slice(0,15))
//pruebasTurbo(ss1_16)
//pruebasTurbo(ss2_1024)
//pruebasTurbo(ss2_2048)
//pruebasTurbo(ss2_4096)
//
//pruebasTurboMejorada(ss1_10)
//pruebasTurboMejorada(ss1_16.slice(0,10))
//pruebasTurboMejorada(ss1_16.slice(0,11))
//pruebasTurboMejorada(ss1_16.slice(0,12))
//pruebasTurboMejorada(ss1_16.slice(0,13))
//pruebasTurboMejorada(ss1_16.slice(0,14))
//pruebasTurboMejorada(ss1_16.slice(0,15))
//pruebasTurboMejorada(ss1_16)
//pruebasTurboMejorada(ss2_1024)
//pruebasTurboMejorada(ss2_2048)
//pruebasTurboMejorada(ss2_4096)
//
//pruebasTurboAcelerada(ss1_10)
//pruebasTurboAcelerada(ss1_16.slice(0,10))
//pruebasTurboAcelerada(ss1_16.slice(0,11))
//pruebasTurboAcelerada(ss1_16.slice(0,12))
//pruebasTurboAcelerada(ss1_16.slice(0,13))
//pruebasTurboAcelerada(ss1_16.slice(0,14))
//pruebasTurboAcelerada(ss1_16.slice(0,15))
//pruebasTurboAcelerada(ss1_16)
//pruebasTurboAcelerada(ss2_1024)
//pruebasTurboAcelerada(ss2_2048)
//pruebasTurboAcelerada(ss2_4096)

// PRUEBAS PROPUESTAS: GRUPO 1

// Pruebas diagnósticas: Resultado y Rendimiento

val s1_4 = secAlAzar(4,Seq())
reconstruirCadenaIngenuo(s1_4.length, crearOraculo(costoOraculo)(s1_4))
reconstruirCadenaIngenuoPar(2)(s1_4.length, crearOraculo(costoOraculo)(s1_4))
print(s1_8) // Resultado esperado: Tamano 8
reconstruirCadenaIngenuo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaIngenuoPar(2)(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurbo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboPar(2)(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboMejorada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboMejoradaPar(2)(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboAcelerada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboAceleradaPar(2)(s1_8.length, crearOraculo(costoOraculo)(s1_8))
print(s1_16) // Resultado esperado: Tamano 16
reconstruirCadenaTurbo(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboPar(2)(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboMejorada(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboMejoradaPar(2)(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboAcelerada(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboAceleradaPar(2)(s1_16.length, crearOraculo(costoOraculo)(s1_16))
print(s1_32) // Resultado esperado: Tamano 32
reconstruirCadenaTurbo(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurboPar(2)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurboMejorada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurboMejoradaPar(2)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurboAcelerada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurboAceleradaPar(2)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
print(s1_64) // Resultado esperado: Tamano 64
reconstruirCadenaTurbo(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurboPar(2)(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurboMejorada(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurboMejoradaPar(2)(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurboAcelerada(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurboAceleradaPar(2)(s1_64.length, crearOraculo(costoOraculo)(s1_64))

compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(2))(s1_4.length, crearOraculo(costoOraculo)(s1_4))
compararAlgoritmos(reconstruirCadenaMejorado,reconstruirCadenaMejoradoPar(2))(s1_4.length, crearOraculo(costoOraculo)(s1_4))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(2))(s1_8.length, crearOraculo(costoOraculo)(s1_8))
compararAlgoritmos(reconstruirCadenaMejorado,reconstruirCadenaMejoradoPar(2))(s1_8.length, crearOraculo(costoOraculo)(s1_8))
compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s1_8.length, crearOraculo(costoOraculo)(s1_8))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s1_8.length, crearOraculo(costoOraculo)(s1_8))
compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(2))(s1_8.length, crearOraculo(costoOraculo)(s1_8))
compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s1_16.length, crearOraculo(costoOraculo)(s1_16))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s1_16.length, crearOraculo(costoOraculo)(s1_16))
compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(2))(s1_16.length, crearOraculo(costoOraculo)(s1_16))
compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s1_32.length, crearOraculo(costoOraculo)(s1_32))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s1_32.length, crearOraculo(costoOraculo)(s1_32))
compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(2))(s1_32.length, crearOraculo(costoOraculo)(s1_32))
compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s1_64.length, crearOraculo(costoOraculo)(s1_64))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s1_64.length, crearOraculo(costoOraculo)(s1_64))
compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(2))(s1_64.length, crearOraculo(costoOraculo)(s1_64))
compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s1_128.length, crearOraculo(costoOraculo)(s1_128))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s1_128.length, crearOraculo(costoOraculo)(s1_128))
compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(2))(s1_128.length, crearOraculo(costoOraculo)(s1_128))
compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s1_256.length, crearOraculo(costoOraculo)(s1_256))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s1_256.length, crearOraculo(costoOraculo)(s1_256))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAceleradaPar(2))(s1_256.length, crearOraculo(costoOraculo)(s1_256))
compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s1_512.length, crearOraculo(costoOraculo)(s1_512))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s1_512.length, crearOraculo(costoOraculo)(s1_512))
compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAceleradaPar(2))(s1_512.length, crearOraculo(costoOraculo)(s1_512))

// Pruebas de Rendimiento: Versiones secuenciales vs versiones paralelas.

val costoOraculo = 1

for {
  noPrueba <- 1 to 4
  i <- 1 to 3
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar(2))(s.length,o),
  math.pow(2, i).toInt)

// Descartamos reconstruirCadenaIngenuo de las pruebas con secuencias de tamaño: 2^4 a 2^10
for {
  noPrueba <- 1 to 4
  i <- 1 to 7
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(2))(s.length,o),
  math.pow(2, i).toInt)

// Descartamos reconstruirCadenaMejorado de las pruebas con secuencias de tamaño: 2^8 a 2^9
for {
  noPrueba <- 1 to 4
  i <- 1 to 8
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(s.length,o),
  math.pow(2, i).toInt)

// Descartamos reconstruirCadenaTurbo de las pruebas con secuencias de tamaño: 2^9 a 2^10
for {
  noPrueba <- 1 to 4
  i <- 1 to 10
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(2))(s.length,o),
  math.pow(2, i).toInt)

for {
  noPrueba <- 1 to 4
  i <- 1 to 10
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(2))(s.length,o),
  math.pow(2, i).toInt)

// Pruebas de Rendimiento para análisis comparativo: Pruebas entre algoritmos.
// Determinamos un rango de tamaños, en el que operen la mayoría de algoritmos en un tiempo moderado: 2^4 a 2^7
// NOTA: Para percibir todo el "output" se debe establecer un rango de pruebas menor, dependiendo la cantidad
// de comparaciones a realizar. Según lo estimado 15 comparaciones sería lo máximo que logra presentar la consola
// al momento dee imprimir los resultados. Por lo que en las pruebas a realizar se tuvo que realizar 1 o 2 pruebas
// a la vez, en cada bucle. Se deja: noPrueba <- 1 to 4, para ilustrar que 4 fueron las pruebas que se realizaron
// para cada bucle.

// Entre secuenciales
for {
  noPrueba <- 1 to 4
  i <- 1 to 7
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (4, compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaTurbo)(s.length,o),
  math.pow(2, i).toInt)

for {
  noPrueba <- 1 to 4
  i <- 1 to 7
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (4, compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboMejorada)(s.length,o),
  math.pow(2, i).toInt)

for {
  noPrueba <- 1 to 4
  i <- 1 to 7
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (4, compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s.length,o),
  math.pow(2, i).toInt)

// Entre paralelos: Aumentamos el rango para examinar como escala su rendimiento.
for {
  noPrueba <- 1 to 4
  i <- 4 to 9
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaMejoradoPar(2), reconstruirCadenaTurboPar(2))(s.length,o),
  math.pow(2, i).toInt)

for {
  noPrueba <- 1 to 4
  i <- 4 to 9
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaTurboPar(2), reconstruirCadenaTurboMejoradaPar(2))(s.length,o),
  math.pow(2, i).toInt)

for {
  noPrueba <- 1 to 4
  i <- 4 to 10
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaTurboMejoradaPar(2), reconstruirCadenaTurboAceleradaPar(2))(s.length,o),
  math.pow(2, i).toInt)

// Entre TurboAcelerada secuencial y MejoradoPar

for {
  noPrueba <- 1 to 4
  i <- 4 to 9
  s = secAlAzar(math.pow(2, i).toInt, Seq())
  o = crearOraculo(costoOraculo)(s)
} yield (noPrueba, compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaMejoradoPar(2))(s.length,o),
  math.pow(2, i).toInt)

// Pruebas de pertenece():
val t = Nodo(' ', false, List(
  Nodo('a', false, List(
    Nodo('c', true, List(
      Nodo('a', false, List(
        Hoja('c', true)
      )),
      Hoja('t', true)
    ))
  )),
  Nodo('c', true, List(
    Nodo('a', false, List(
      Nodo('c', true, List(
        Hoja('t', true)
      ))
    )),
    Hoja('t',true)
  )),
  Hoja('t', true)
))

val secuencia1 = Seq('a','c')
pertenece(secuencia1,t)
val secuencia2 = Seq('a','c','t')
pertenece(secuencia2,t)
val secuencia3 = Seq('c','a','c','t')
pertenece(secuencia3,t)
val secuencia4 = Seq('t')
pertenece(secuencia4,t)
val secuencia5 = Seq('a','c','a','c')
pertenece(secuencia5,t)
val secuencia6 = Seq('c','a')
pertenece(secuencia6,t)
val secuencia7 = Seq('a','c','a')
pertenece(secuencia7,t)
val secuencia8 = Seq('g','t')
pertenece(secuencia8,t)
val secuencia9 = Seq('g') // 'g' no está en el árbol de sufijos t.
pertenece(secuencia9,t)
val secuencia10 = Seq(' ') // ' ' no está en el diccionario.
pertenece(secuencia10,t)

// Pruebas de adicionar():
val secuencia11 = Seq('a','c','a','c','t')
val secuencia12 = Seq('g','t')
val secuencia13 = Seq('g','t','a')
val t2 = adicionar(secuencia9,t)
val t3 = adicionar(secuencia11,t)
val t4 = adicionar(secuencia12,t2)
val t5 = adicionar(secuencia13,t4)
val t6 = adicionar(secuencia13,t)
val t7 = adicionar(secuencia6,t)
print(t)
print(t2)
print(t3)
print(t4)
print(t5)
print(t6)
pertenece(secuencia11,t3)
pertenece(secuencia8,t5)
pertenece(secuencia13,t5)
pertenece(secuencia13,t6)
pertenece(secuencia6,t7)

// Pruebas de arbolDeSufijos():
val ss = Seq(Seq('c'),Seq('t'),Seq('c','t'),Seq('a','c'),Seq('a','c','t'),Seq('c','a','c'),Seq('a','c','a','c'),Seq('c','a','c','t'))
val t_reconstruido = arbolDeSufijos(ss)
print(t_reconstruido)
print(t)