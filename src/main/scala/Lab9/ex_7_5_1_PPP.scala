package Lab8

import com.cra.figaro.language.Select
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.language.{Apply, Chain, Element}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import scala.collection.IterableOnceExtensionMethods

""" 7.5.1. Your firm has five departments: Research and Development, Production, Sales,
Human Resources, and Finance. Build an object-oriented probabilistic model
that captures the influences between these departments. Use the model to answer
queries about the health of the firm based on the state of the departments."""

object ex_7_5_1_PPP extends ElementCollection {
  def main(args: Array[String])
  {
    // class for the products produced by the company
    class Product {
      val prod_ID : Element[Int]
      val sold_units : Element[Int]
      val price : Element[Int]
      val production_cost: Element[Double]
//      def getSoldUnits(): Unit = {
//        return sold_units
//      }
//      def updateSoldUnits(updatedNumber: Int): Unit = {
//        sold_units = updatedNumber
//      }
    }

    object Product{
      override val prod_ID = Uniform(1, 2, 3)
      override val sold_units = Uniform(10000, 90000)
      override val price = Uniform(30, 100) //products will have a price within (30,100)
      override val production_cost = Uniform(10.0, 29.9)
      // display method
      def Display(): Unit = {
        println("Product ID: " + prod_ID)
        prinln("Number of sold units: " + sold_units)
        println("Price per unit: " + price)
        prinln("Production cost: " + production_cost)
      }
    }

    // class for department - interface
    trait Department{
      // properties
      val ID: Element[Int]
      val numberOfEmployees: Element[Int]
      val requiredEmployees: Element[Int]
      val salary: Element[Int]
      val registeredComplaints: Element[Int]

      // getters
      def getNumberOfEmployees(ID: Int) = numberOfEmployees
      def getID(): Unit = {
        return ID
      }

      // setters
      def setID(departmentID: Int): Unit = {
        ID = departmentID
      }
      def setNumberOfEmployees(number: Int): Unit = {
        numberOfEmployees = number
      }
    }


    // class for research and development department extending class Department
    case class ResearchAndDevelopment extends Department {
      //public constructor
      def this() {
        override val ID = setID(1)
        override val numberOfEmployees = Uniform(20, 40)
        override val requiredEmployees = 27
        override val salary = 5000
        override val registeredComplaints = Uniform(0, numberOfEmployees)
      }

      // method designed to analyze product and see if its production cost is at least 30% smaller than the price it's sold at
      def product_is_viable(Product: prod): Boolean {
        return (prod.price <= prod.production_cost + prod.production_cost * 0.3)
      }

      // method for suggesting new price for product
      // if sold_units > 30000 --> suggested price = old price increased by 10%
      def change_price_for_product(Product: prod): Int {
        val current_price = prod.price
        if (prod.sold_units >= 30000)
          return prod.price * 0.1 + prod.price
      }
    }


    //class for production department extending class Department
    case class Production extends Department {
      //public constructor
      def this() {
        override val ID = setID(2)
        override val numberOfEmployees = Uniform(40, 70)
        override val requiredEmployees = 70
        override val salary = Uniform(4000, 6700)
        override val registeredComplaints = Uniform(0, numberOfEmployees)
      }
      // production department creates products sold by the company (we will consider 3 such products)

      // create 3 products
      val prod1 = new Product
      val prod2 = new Product
      val prod3 = new Product

      // method for determining total gain (in currency)
      // gain = sum(product_sold_units * price)
      def calculateGain(): Unit = {
        return (prod1.sold_units * prod1.price) + (prod2.sold_units * prod2.price) + (prod3.sold_units * prod3.price)
      }
    }

    // class for sales department extending class Department
    case class Sales extends Department with Production {
      //public constructor
      def this() {
        override val ID = setID(3)
        override val numberOfEmployees = Uniform(30, 45)
        override val requiredEmployees = 50
        override val salary = Uniform(3500, 7000)
        override val registeredComplaintsFromEmployees = 4
      }

      //function for determining number of sold products
      def getSoldUnitsTotal(): Unit = {
        val sold_units_in_total = prod1.sold_units + prod2.sold_units + prod3.sold_units
        return sold_units_in_total
      }
    }

    // class for HR departments extending class Department
    case class HR extends Department {
      //public constructor
      def this() {
        override val ID = setID(4)
        override val numberOfEmployees = Uniform(10, 30)
        override val requiredEmployees = 15
        override val salary = Uniform(3000, 5000)
        override val registeredComplaints = Uniform(0, numberOfEmployees)
      }

      // HR will decide on the hiring of a new employee
      def hireEmployeeForDepartment(departmentID: Int) {
        // increase number of employees for that department by 1
        numberOfEmployees = setNumberOfEmployees(getNumberOfEmployees(departmentID) + 1)
      }

      // HR will decide if a department works well based on its number of registered complaints
      // if number of registered complains < 10% of number of employees --> okay (true)
      // else --> needs assistance (false)
      def analyze_department(departmentID: Int) : Boolean {
        if (registeredComplaints <= 0.1 * numberOfEmployees)
          return true
        else return false
      }
    }


    // class for finance department extending class Department
    case class Finance extends Department {
      //public constructor
      def this() {
        override val ID = setID(5)
        override val numberOfEmployees = Uniform(3,10)
        override val requiredEmployees = 5
        override val salary = Uniform(4200, 5600)
        override val registeredComplaints = Uniform(0, numberOfEmployees)
       }

      // finance department can change employee salary
      // if HR department considers the department works well --> 15% increase
      // else --> do nothing
      def SetNewSalaryForDepartment(departmentID: Int) {
        val increased_salary = salary + salary * 0.15
        val HR_obj = new HR
        if (HR_obj.analyze_department == true)
          this.salary = increased_salary
      }

      // method for determining profit
      // profit = gain (calculated by the sales department) -
    }
  }
}

//object Main() {
//  // Main method
//  def main(args: Array[String]) {
//
//  }
//}