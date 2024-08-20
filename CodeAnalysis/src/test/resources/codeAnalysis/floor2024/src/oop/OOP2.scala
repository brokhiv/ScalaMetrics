package floor2024.oop

import scala.collection.mutable.ListBuffer

object OOP2:
  /*
   * What is the functionality of funcA, and what does fundB do?
   */
  
  def funcA(listA: List[Int]): List[Int] =
    if listA.size <= 1 then
      return listA
    
    val varA = listA.size / 2
    val listB = listA.slice(0, varA)
    val listC = listA.slice(varA, listA.size)
    
    val listD = funcA(listB)
    val listE = funcA(listC)
    
    funcB(listD, listE)
  
  private def funcB(listA: List[Int], listB: List[Int]): List[Int] =
    var i = 0
    var j = 0
    val listC = ListBuffer[Int]()
    
    while i < listA.size && j < listB.size do
      if listA(i) < listB(j) then 
        listC.addOne(listA(i))
        i += 1
      else 
        listC.addOne(listB(j))
        j += 1
    
    while i < listA.size do
      listC.addOne(listA(i))
      i += 1
    
    while j < listB.size do
      listC.addOne(listB(j))
      j += 1
    
    listC.toList
