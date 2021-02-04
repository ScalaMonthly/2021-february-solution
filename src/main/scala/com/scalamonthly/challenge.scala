package com.scalamonthly

import cats.data.NonEmptyList

object challenge {

  final case class EmployeeInfo(name: String, salesRevenue: Int, salary: Int)
  sealed abstract class Employee extends Product with Serializable {
    import com.scalamonthly.challenge.Employee._
    def fold[A](ic: EmployeeInfo => A)(m: (EmployeeInfo, List[A]) => A): A = this match {
      case Manager(info, directReports) => m(info, directReports.map(_.fold(ic)(m)))
      case IndividualContributor(info) => ic(info)
    }
    def numOfEmployees: Int = this.fold(_ => 1)((_, emp) => 1 + emp.sum)
    def totalProfit: Int = this.fold(i => i.salesRevenue - i.salary)((i, acc) => acc.sum + (i.salesRevenue - i.salary))
    def profitPerEmployee: Double = {
      val result = this.fold(i => (i.salesRevenue - i.salary, 1))(
        (i, acc) => (acc.map(_._1).sum + (i.salesRevenue - i.salary), acc.map(_._2).sum + 1)
      )
      result._1.toDouble / result._2
    }
  }
  object Employee {
    final case class Manager(info: EmployeeInfo, directReports: List[Employee]) extends Employee
    final case class IndividualContributor(info: EmployeeInfo) extends Employee
  }
  final case class BranchName(value: String) extends AnyVal
  final case class Branch(name: BranchName, manager: Employee.Manager)

  def determineBranchToShutDown(branches: NonEmptyList[Branch]): BranchName = {
    branches.toList.minBy(_.manager.profitPerEmployee).name
  }

}