import org.apache.log4j.Logger

class ListOperations {

  def length[A](list: List[A]): Int = {
    list.foldRight(0)((_, count) => count + 1)
  }

  def concatenateList[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case head :: tail => head :: concatenateList(tail, l2)
    }
  }

  def hasSubSequence[A](list: List[A], sub: List[A]): Boolean = {
    (list, sub) match {
      case (_, Nil) => true
      case (head1 :: tail1, head2 :: tail2) => if (head1 == head2) hasSubSequence(tail1, tail2) else hasSubSequence(tail1, head2 :: tail2)
      case _ => false
    }
  }

  def splitList[A](l: List[A], f: A => Boolean): (List[A], List[A]) = {
    def divider(list: List[A], valid: List[A], invalid: List[A]): (List[A], List[A]) = {
      list match {
        case head :: tail if f(head) => divider(tail, valid :+ head, invalid)
        case head :: tail if !f(head) => divider(tail, valid, invalid :+ head)
        case Nil => (valid, invalid)
      }
    }

    divider(l, List[A](), List[A]())
  }
}


object ListOperations extends App {
  val log = Logger.getLogger(this.getClass)
  val obj = new ListOperations
  val testList: List[Int] = (1 to 10).toList
  val listToAppend: List[Int] = (11 to 15).toList
  log.info(s"${testList}")
  log.info("\n\n1. Finding Length")
  log.info(s"Length of List: ${obj.length(testList)}")
  log.info("\n\n2. Finding If A Sub-Sequence Is Present")
  log.info(s"${testList}\n")
  log.info(s"${listToAppend}\n")
  if(obj.hasSubSequence(testList,listToAppend)) log.info("Present\n") else log.info("Not Present\n")
  log.info("\n\n3. Concatenate Two Lists")
  log.info(s"Concatenated List: ${obj.concatenateList(testList,listToAppend)}")
  log.info(s"\n\n4. Filter elements of list: Odd & Even List\n")
  val (odd, even) = obj.splitList(testList, (number: Int) => number % 2 == 0)
  log.info(s"Odd List : ${odd}\n")
  log.info(s"Even List : ${even}\n")
}
