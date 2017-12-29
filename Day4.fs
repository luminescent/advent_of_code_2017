namespace AdventOfCode


module Day4 = 
    open System 
    open System.IO

    open Microsoft.FSharp.Collections

    let containsDuplicates items = 
        let distinctItems = items |> Set.ofArray

        distinctItems.Count <> items.Length

    let toString : char seq -> string = Seq.map string >> String.concat ""

    let containsAnagrams (items: string[]) = 
        let normalizedDistinctItems = 
            items 
            |> Array.map (Seq.sort >> toString)  
            |> Set.ofArray

        normalizedDistinctItems.Count <> items.Length

    let parseLine (line: string) =
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries)

    let part1 items = 
        items 
        |> Array.map parseLine 
        |> Array.filter (containsDuplicates >> not)
        |> Array.length

    let part2 items = 
        items
        |> Array.map parseLine
        |> Array.filter (containsAnagrams >> not)
        |> Array.length

    let runDay4() = 
        let lines = File.ReadAllLines "Day4.txt"

        lines 
            |> part1 
            |> printfn "Part 1: %A"

        lines 
            |> part2 
            |> printfn "Part 2: %A"
