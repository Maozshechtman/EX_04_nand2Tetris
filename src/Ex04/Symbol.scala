package Ex04

class Symbol(symbolName:String, symbolType:String, symbolkind:String, symbolIndex:Int) {
  val sName:String=symbolName

  val sType:String=symbolType

  val sKind:String=symbolkind

  val sIndex:Int=symbolIndex

  //we don't have have static in scala because scala is pure oop
  //and static does not connect to object but to class
}
