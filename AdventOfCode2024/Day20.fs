/// https://adventofcode.com/2024/day/20
module AdventOfCode2024.Day20

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day20.txt"

let testData =
   seq {
      "###############"
      "#...#...#.....#"
      "#.#.#.#.#.###.#"
      "#S#...#.#.#...#"
      "#######.#.#.###"
      "#######.#.#...#"
      "#######.#.###.#"
      "###..E#...#...#"
      "###.#######.###"
      "#...###...#...#"
      "#.#####.#.###.#"
      "#.#...#.#.#...#"
      "#.#.#.#.#.#.###"
      "#...#...#...###"
      "###############"      
   }

type GridCell =
   | Start
   | End
   | Wall
   | Empty
   
let grid =
   data
   |> Seq.collecti (fun r ->
      Seq.mapi (fun c cell ->
         vec2i r c,
         match cell with
         | 'S' -> Start
         | 'E' -> End
         | '#' -> Wall
         | '.' -> Empty
         | _ -> raise invalidInput))
   |> Map.ofSeq

let startPosition =
   grid |> Map.toSeq |> Seq.find (fun (_, v) -> v = Start) |> fst

let rec search (current : Vector2i) (pathR : Vector2i list) =
   let next =
      seq {
         current - Vector2i.UnitR
         current + Vector2i.UnitR
         current - Vector2i.UnitC
         current + Vector2i.UnitC
      }
      |> Seq.map (fun loc -> loc, grid[loc])
      |> Seq.filter (fun x -> snd x = Empty || snd x = End)
      |> Seq.filter (fun x -> pathR |> List.contains (fst x) |> not)
      |> Seq.tryHead
      |> Option.map fst
      
   match next with
   | Some n -> search n (current :: pathR)
   | None -> (current :: pathR) |> List.rev

let part1 () =
   grid |> printfn "%A"
   
   // let length = grid |> Map.toSeq |> Seq.map snd |> Seq.filter (fun x -> x = Empty || x = End) |> Seq.length
   //
   // printfn "%A" length
   
   let indexedPath = search startPosition [] |> List.indexed
   
   let indexedPathMap = indexedPath |> Seq.map (fun (a, b) -> b, a) |> Map.ofSeq
 
   let shortcuts =
      indexedPath
      |> Seq.collect (fun (dist, current) ->
         indexedPath
         |> Seq.choose (fun cand ->
            let manhattan = Vector2i.ManhattanDistance(snd cand, current)
            if manhattan <= 20 then
               indexedPathMap
               |> Map.tryFind (snd cand)
               |> Option.map (fun x ->
                  let usual = x - dist
                  let shortcut = manhattan
                  usual - shortcut)
            else
               None)
         |> Seq.filter (fun x -> x > 0))
      |> Seq.toList
   
   // shortcuts
   // |> List.countBy id
   // |> List.sortBy fst
   // |> printfn "%A"

   shortcuts
   |> List.filter (fun s -> s >= 100)
   |> List.length
   |> printfn "%A"
   
   0

let part2 () = 0

let part1Expected = 0

let part2Expected = 0
