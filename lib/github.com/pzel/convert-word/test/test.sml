local
  val w8 = Word8.fromLargeInt o Time.toMilliseconds o Time.now
  val w32 = Word32.fromLargeInt o Time.toMilliseconds o Time.now
  val w64 = Word64.fromLargeInt o Time.toMilliseconds o Time.now
  val id = fn x => x
in
val oldApiTests =  [
  It "does word32 conversion to and from bytestring" (
    fn()=>
       let
         val op == = Assert.eq Word32.toString
         val w0 = w32()
       in ConvertWord.bytesToWord32B(ConvertWord.word32ToBytesB w0) == w0
       end)
  ,It "does word32 conversion to bytestring" (
    fn()=>
       let
         val op == = Assert.eq id
         val w0 = 0wxabcd5678
         val hex = Bytestring.toStringHex
       in hex (ConvertWord.word32ToBytesB w0) == "abcd5678"
       end)
  ,It "does word32 conversion from bytestring" (
    fn()=>
       let
         val op == = Assert.eq Word32.toString
         val b0 = Option.valOf (Bytestring.fromStringHex "5678dcba")
       in ConvertWord.bytesToWord32B b0 == 0wx5678dcba
       end)

  ,It "does word64 conversion to and from bytestring" (
    fn()=>
       let
         val op == = Assert.eq Word64.toString
         val w0 = w64()
       in ConvertWord.bytesToWord64B(ConvertWord.word64ToBytesB w0) == w0
       end)
  ,It "does word64 conversion to bytestring" (
    fn()=>
       let
         val op == = Assert.eq id
         val w0 = 0wx12345678abcdabcd
         val hex = Bytestring.toStringHex
       in hex (ConvertWord.word64ToBytesB w0) == "12345678abcdabcd"
       end)
  ,It "does word64 conversion from bytestring" (
    fn()=>
       let
         val op == = Assert.eq Word64.toString
         val b0 = Option.valOf (Bytestring.fromStringHex "01230123fefebaba")
       in ConvertWord.bytesToWord64B b0 == 0wx1230123FEFEBABA
       end)
]

val newApiTests = [
  It "converts bytestring to word32 option (success)"(
    fn()=>
       let
         val b0 = Option.valOf (Bytestring.fromStringHex "fefebaba")
       in ConvertWord.bytesToWord32B' b0 == SOME 0wxFEFEBABA
       end)
 ,It "converts bytestring to word32 option (failure)"(
    fn()=>
       let
         val b0 = Option.valOf (Bytestring.fromStringHex "32")
       in ConvertWord.bytesToWord32B' b0 == NONE
       end)

 ,It "converts bytestring to word64 option (success)"(
    fn()=>
       let
         val b0 = Option.valOf (Bytestring.fromStringHex "01230123fefebaba")
       in ConvertWord.bytesToWord64B' b0 == SOME 0wx1230123FEFEBABA
       end)
 ,It "converts bytestring to word64 option (failure)"(
    fn()=>
       let
         val b0 = Option.valOf (Bytestring.fromStringHex "32")
       in ConvertWord.bytesToWord64B' b0 == NONE
       end)


]
end

fun main () =
	runTestsWith (oldApiTests @ newApiTests) (CommandLine.arguments())
