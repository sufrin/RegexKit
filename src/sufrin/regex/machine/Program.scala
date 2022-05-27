package sufrin.regex.machine

object Program {

  class  Builder[T] extends collection.mutable.ArrayBuffer[Instruction[T]] {
    def toProgram: Program[T] = toIndexedSeq
  }

  type Program[T] = collection.immutable.IndexedSeq[Instruction[T]]

}