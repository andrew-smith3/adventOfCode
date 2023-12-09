open System
open System.Collections
let test = "A0016C880162017C3686B18A3D4780"

let input = "020D708041258C0B4C683E61F674A1401595CC3DE669AC4FB7BEFEE840182CDF033401296F44367F938371802D2CC9801A980021304609C431007239C2C860400F7C36B005E446A44662A2805925FF96CBCE0033C5736D13D9CFCDC001C89BF57505799C0D1802D2639801A900021105A3A43C1007A1EC368A72D86130057401782F25B9054B94B003013EDF34133218A00D4A6F1985624B331FE359C354F7EB64A8524027D4DEB785CA00D540010D8E9132270803F1CA1D416200FDAC01697DCEB43D9DC5F6B7239CCA7557200986C013912598FF0BE4DFCC012C0091E7EFFA6E44123CE74624FBA01001328C01C8FF06E0A9803D1FA3343E3007A1641684C600B47DE009024ED7DD9564ED7DD940C017A00AF26654F76B5C62C65295B1B4ED8C1804DD979E2B13A97029CFCB3F1F96F28CE43318560F8400E2CAA5D80270FA1C90099D3D41BE00DD00010B893132108002131662342D91AFCA6330001073EA2E0054BC098804B5C00CC667B79727FF646267FA9E3971C96E71E8C00D911A9C738EC401A6CBEA33BC09B8015697BB7CD746E4A9FD4BB5613004BC01598EEE96EF755149B9A049D80480230C0041E514A51467D226E692801F049F73287F7AC29CB453E4B1FDE1F624100203368B3670200C46E93D13CAD11A6673B63A42600C00021119E304271006A30C3B844200E45F8A306C8037C9CA6FF850B004A459672B5C4E66A80090CC4F31E1D80193E60068801EC056498012804C58011BEC0414A00EF46005880162006800A3460073007B620070801E801073002B2C0055CEE9BC801DC9F5B913587D2C90600E4D93CE1A4DB51007E7399B066802339EEC65F519CF7632FAB900A45398C4A45B401AB8803506A2E4300004262AC13866401434D984CA4490ACA81CC0FB008B93764F9A8AE4F7ABED6B293330D46B7969998021C9EEF67C97BAC122822017C1C9FA0745B930D9C480"

type BitArray with

    member x.GetSlice(start: int option, finish: int option) =
        let s = Option.defaultValue 0 start
        let f = Option.defaultValue (x.Length - 1) finish
        [| for i in s .. f do x.Get(i)|]
        |>  BitArray
        
    member x.ToUInt32() =
        let mutable v = 0u
        for i in 0 .. x.Length - 1 do
            v <- (v <<< 1) ||| (if x[i] then 1u else 0u)
        v
        
    member x.Print() =
        seq { for i in 0 .. x.Length - 1 do x.Get(i) }
        |> Seq.map (fun x -> if x then "1" else "0")
        |> String.concat ""
        |> fun s -> printfn $"%s{s}"
        
type Packet = {
    Version: uint
    Type: uint
    Packets: Packet list
    Length: int
}

let getPosition (i: int) =
    let multiple = (i / 8 * 8)
    let offset = 8 - (i % 8) - 1
    multiple + offset

let correctBitOrder (b: BitArray) =
    [| for i in 0 .. b.Length - 1 do b.Get(getPosition i)|]
    |> BitArray

let fpDecode (hex:string) =
    let hexToByte (cs:char[]) = Convert.ToByte(String(cs), 16)
    Seq.chunkBySize 2 hex
    |> Seq.map hexToByte
    |> Seq.toArray
    |> BitArray
    |> correctBitOrder
    
let setBitsForLiteral (initial: uint) (b: bool[]) =
    let mutable v = initial
    for i in 1 .. b.Length - 1 do
        v <- (v <<< 1) ||| (if b[i] then 1u else 0u)
    v

let literalFold ((value, length, foundLast): uint * int * bool) (bits: bool[]) =
    if foundLast
    then (value, length, foundLast)
    else
        let v = setBitsForLiteral value bits
        (v, length + bits.Length, bits[0] = false)

let rec decodePacket (b: BitArray) : Packet =
    let version = b[0..2].ToUInt32()
    let typeHeader = b[3..5].ToUInt32()
    match typeHeader with
    | 4u ->
        seq {for i in 6..b.Length - 1 do b.Get(i)}
        |> Seq.chunkBySize 5
        |> Seq.fold literalFold (0u,6, false)
        |> fun (_, l, _) -> {Version = version; Type = typeHeader; Packets = []; Length = l}
    | _ ->
        let lengthType = b[6]
        match lengthType with
        | true ->
            let numberOfPackets = b[7..17].ToUInt32() |> int
            let mutable startingIndex = 18
            let mutable packets = []
            while packets.Length <> numberOfPackets do
                let nextPacket = decodePacket b[startingIndex..]
                startingIndex <- startingIndex + nextPacket.Length
                packets <- nextPacket::packets
            let length = packets |> List.sumBy (fun x -> x.Length) |> (+) 18
            {Version = version; Type = typeHeader; Packets = packets; Length = length}
        | false ->
            let packetBits = b[7..21].ToUInt32() |> int
            let mutable subpacketBits = b[22..(22 + packetBits - 1)]
            let mutable usedBits = 0
            let mutable packets = []
            while usedBits <> packetBits do
                let nextPacket = decodePacket subpacketBits
                usedBits <- usedBits + nextPacket.Length
                subpacketBits <- b[(22 + usedBits)..(22 + packetBits - 1)]
                packets <- nextPacket::packets
            let length = packets |> List.sumBy (fun x -> x.Length) |> (+) 22
            {Version = version; Type = typeHeader; Packets = packets; Length = length}

let rec sumVersion (p: Packet) =
    match p.Packets with
    | [] -> p.Version
    | x ->
        x
        |> List.sumBy sumVersion
        |> (+) p.Version
            

let main (input: string) =
    input
    |> fpDecode
    |> decodePacket
    |> sumVersion
    |> fun x -> printfn $"Answer: %d{x}"

    
main input