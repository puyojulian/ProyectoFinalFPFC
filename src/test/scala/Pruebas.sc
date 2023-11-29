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
val costoOraculo = 1

val sec1=Seq('a', 'c', 'c', 'a')
val sec2 = Seq('a', 'c', 'g', 'c', 'a')
val sec3=secAlAzar(10,Seq())

val or_1=crearOraculo(costoOraculo)(sec1)
val or_2=crearOraculo(costoOraculo)(sec2)
val or_3=crearOraculo(costoOraculo)(sec3)

// Pruebas con reconstruirCadenaIngenuoPar
val umbral1 = 2
val umbral2 = 4
val umbral3 = 8

//reconstruirCadenaIngenuoPar(umbral1)(sec1.length, or_1)

reconstruirCadenaIngenuo(sec1.length, or_1)
reconstruirCadenaIngenuo(sec2.length, or_2)
//reconstruirCadenaIngenuo(sec3.length, or_3)
reconstruirCadenaMejorado(sec3.length, or_3)


def secsCortasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(i,Seq())
} yield s

def secsLargasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(math.pow(2,i).toInt,Seq())
} yield s

def pruebasIngenuo(ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s,reconstruirCadenaIngenuo(s.length,o))



//def pruebasMejorado (ss:Seq[Seq[Char]]) = for {
//  s <- ss
//  o = crearOraculo(costoOraculo)(s)
//} yield (s.length, s,reconstruirCadenaMejorado(s.length,o))
//
//
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
//
//
//def pruebasTurboAcelerada(ss:Seq[Seq[Char]]) = for {
//  s <- ss
//  o = crearOraculo(costoOraculo)(s)
//} yield (s.length, s,reconstruirCadenaTurboAcelerada(s.length,o))


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
val s1_1024 = ss2_1024(9)
val s2_1024 = ss2_2048(9)
val s3_1024 = ss2_4096(9)
val s1_2048 = ss2_2048(10)
val s2_2048 = ss2_4096(10)
val s1_4096 = ss2_4096(11)

val resultadoguia = reconstruirCadenaTurbo(s1_32.length, crearOraculo(costoOraculo)(s1_32))
val resultaprueba2 = reconstruirCadenaTurboPar(1)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
val resultadoguia1 = reconstruirCadenaTurboMejorada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
val resultadoprueba3 = reconstruirCadenaTurboMejoradaPar(1)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
val resultadoguia3 = reconstruirCadenaTurboAcelerada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
val resultadoguia4 = reconstruirCadenaTurboAceleradaPar(1)(s1_32.length, crearOraculo(costoOraculo)(s1_32))

//val resultadoprueba4 = reconstruirCadenaTurboMejoradaParWop(1)(s1_32.length, crearOraculo(costoOraculo)(s1_32))

// Pruebas funcionales

// secuencias de longitud 8
//reconstruirCadenaIngenuo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//reconstruirCadenaIngenuo(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaMejorado(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaMejorado(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaTurbo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurbo(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaTurboMejorada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboMejorada(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaTurboAcelerada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboAcelerada(s2_8.length, crearOraculo(costoOraculo)(s2_8))

//// secuencias de longitud 16
reconstruirCadenaMejorado(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurbo(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboMejorada(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboAcelerada(s1_16.length, crearOraculo(costoOraculo)(s1_16))

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

// Pruebas de Rendimiento:
//compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaTurbo)(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_8.length, crearOraculo(costoOraculo)(s1_8))
//compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaTurbo)(s1_16.length, crearOraculo(costoOraculo)(s1_16))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_16.length, crearOraculo(costoOraculo)(s1_16))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_16.length, crearOraculo(costoOraculo)(s1_16))
//compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaTurbo)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_32.length, crearOraculo(costoOraculo)(s1_32))
//compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaTurbo)(s1_64.length, crearOraculo(costoOraculo)(s1_64))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_64.length, crearOraculo(costoOraculo)(s1_64))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_64.length, crearOraculo(costoOraculo)(s1_64))
//compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaTurbo)(s1_128.length, crearOraculo(costoOraculo)(s1_128))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_128.length, crearOraculo(costoOraculo)(s1_128))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_128.length, crearOraculo(costoOraculo)(s1_128))
//compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaTurbo)(s1_256.length, crearOraculo(costoOraculo)(s1_256))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_256.length, crearOraculo(costoOraculo)(s1_256))
//compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboAcelerada)(s1_256.length, crearOraculo(costoOraculo)(s1_256))

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
pertenece(secuencia6,adicionar(secuencia6,t))

// Pruebas de arbolDeSufijos():
val ss = Seq(Seq('c'),Seq('t'),Seq('c','t'),Seq('a','c'),Seq('a','c','t'),Seq('c','a','c'),Seq('a','c','a','c'),Seq('c','a','c','t'))
val t_reconstruido = arbolDeSufijos(ss)
print(t_reconstruido)
print(t)

def crearRama(s: Seq[Char]): Trie = {
  s match {
    case cabeza :: cola => cola match {
      case head :: tail => Nodo(cabeza, false, List(crearRama(cola)))
      case Nil => Hoja(cabeza, true)
    }
    case Nil => Nodo(' ', false, List())
  }
}

def dividirSecuencia(s: Seq[Char], t: Trie): (Seq[Char], Seq[Char]) = {
  val (parteReconocida, parteNoReconocida) = s.foldLeft((Seq.empty[Char], Seq.empty[Char])) {
    case ((accPrefix, accRemaining), char) =>
      if (perteneceLaxa(accPrefix :+ char, t))
        (accPrefix :+ char, accRemaining)
      else
        (accPrefix, accRemaining :+ char)
  }
  (parteReconocida, parteNoReconocida)
}

val (secuenciaEnArbol, secuenciaNoEnArbol) = dividirSecuencia(secuencia6, adicionar(secuencia6,t))
val nuevaRama = crearRama(secuenciaNoEnArbol)

val nuevaRama = crearRama(Seq())