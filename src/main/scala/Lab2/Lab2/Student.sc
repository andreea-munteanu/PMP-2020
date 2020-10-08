import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Student extends Persoana {

  var nume:String
  var prenume:String
  var an:Int
  var id:String
  var materii: ArrayBuffer[Materie]

  def createStudentandGrades(gender:Char, age:Int, address:String, nume:String, prenume: String, an: Int, id: String): Unit = {
    this.gender = gender
    this.age = age
    this.address = address
    this.nume = nume
    this.prenume = prenume
    this.an = an
    this.id = id
    for (i <- materii) {
      materii(i).nota = Random.between(4, 11) // in [20, 30[
    }

  }

  def getStudentInfo() : Unit =  {
    println(id + ",   " + nume + ' ' + prenume);
  }

  def setNota(materie:String, nota:Int) : Unit = {
    for (i <- materii) {
      if (materii(i).materie == materie) {
        materii(i).nota = nota
      }
    }
    throw new Error("Materia nu exista!")
  }

  def getNota(materie:String) {
    for (i <- materii) {
      if (materii(i).materie == materie) {
        return materii(i).nota
      }
    }
    throw new Error("Nu exista nota!")
  }

  def addMaterie(materie: String, nota: Integer) {
    materii +:= (materie,nota)
  }

}
