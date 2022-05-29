package sufrin.regex.machine

object Program {
  /** A program builder enables instructions to be added in sequence,
   *  and labels to be added defined.
   */
  class  Builder[T] extends collection.mutable.ArrayBuffer[Instruction[T]] {
    def toProgram: Program[T]       = toArray
    def define(label: Lab[T]): Unit = label.loc = length
  }

  type Program[T] = Array[Instruction[T]] // collection.immutable.IndexedSeq[Instruction[T]]

}