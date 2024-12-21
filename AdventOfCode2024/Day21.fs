/// https://adventofcode.com/2024/day/21
module AdventOfCode2024.Day21

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day21.txt"

let testData =
   seq {
      "029A"
      "980A"
      "179A"
      "456A"
      "379A"      
   }

type Key =
   | NumKey of int
   | AKey
   | BlankKey
   | LeftKey
   | RightKey
   | UpKey
   | DownKey

let codes =
   data
   |> Seq.map (fun line ->
      let keyCode =
         line
         |> Seq.map (function
            | '0' -> NumKey 0
            | '1' -> NumKey 1
            | '2' -> NumKey 2
            | '3' -> NumKey 3
            | '4' -> NumKey 4
            | '5' -> NumKey 5
            | '6' -> NumKey 6
            | '7' -> NumKey 7
            | '8' -> NumKey 8
            | '9' -> NumKey 9
            | 'A' -> AKey
            | _ -> raise invalidInput)
         |> Seq.toList
         
      let numeric =
         match line with
         | Regex "^(\d+)" [ Int64 numericPart ] -> numericPart
         | _ -> raise invalidInput
      
      keyCode, numeric)
   |> Seq.toList
   
let numberPad =
   [| [| NumKey 7; NumKey 8; NumKey 9 |]
      [| NumKey 4; NumKey 5; NumKey 6 |]
      [| NumKey 1; NumKey 2; NumKey 3 |]
      [| BlankKey; NumKey 0; AKey     |] |]
   |> Seq.collecti (fun r -> Seq.mapi (fun c key -> vec2i r c, key))
   |> Map.ofSeq
   
let directionPad =
   [| [| BlankKey; UpKey;    AKey     |]
      [| LeftKey;  DownKey;  RightKey |] |]
   |> Seq.collecti (fun r -> Seq.mapi (fun c key -> vec2i r c, key))
   |> Map.ofSeq
   
let findKey key pad =
   pad |> Map.toSeq |> Seq.find (fun (_, v) -> v = key) |> fst  

let costM =
   Func.memoizeRec (fun costRec (fromKey : Key, toKey : Key, depth : int, maxDepth : int, pad : Map<Vector2i, Key>) ->
      if depth = maxDepth then
         1L
      else
         let fromPos, toPos = (fromKey, toKey) |> mapBoth (flip findKey pad)
         let horz, vert =
            let delta = toPos - fromPos
            List.replicate (abs delta.C) (if delta.C > 0 then RightKey else LeftKey),
            List.replicate (abs delta.R) (if delta.R > 0 then DownKey else UpKey)
         [ if pad[vec2i fromPos.R toPos.C] <> BlankKey then [ AKey; yield! horz; yield! vert; AKey ]
           if pad[vec2i toPos.R fromPos.C] <> BlankKey then [ AKey; yield! vert; yield! horz; AKey ] ]
         |> Seq.map (Seq.pairwise >> Seq.sumBy (fun (fromKey, toKey) -> costRec (fromKey, toKey, depth + 1, maxDepth, directionPad)))
         |> Seq.min)

let solve maxDepth =
   let cost (fromKey, toKey) =
      costM (fromKey, toKey, 0, maxDepth, numberPad)
   
   codes
   |> Seq.sumBy (fun (codeKeys, codeNumeric) ->
      codeNumeric * (AKey :: codeKeys |> Seq.pairwise |> Seq.sumBy cost))

let part1 () = solve 3

let part2 () = solve 26

let part1Expected = 177814L

let part2Expected = 220493992841852L
