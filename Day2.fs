namespace AdventOfCode

open System
open System.IO

module Day2 = 

    let rowDiff row = 
        (row
        |> Array.max) 
        - 
        (row
        |> Array.min) 

    let parseLine (line: string) =
        line.Split('\t', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> Convert.ToInt32(s))

    let parseLines lines = 
        lines 
        |> Array.map parseLine

    let computeChecksum lines = 
        lines
        |> parseLines
        |> Array.sumBy rowDiff

    let rec rowDivision row = 
        match row with
        | head :: tail -> 
            let possibleSmaller = tail |> List.tryFind(fun a -> a < head && a <> 0 && head % a = 0)
            match possibleSmaller with 
            | Some x -> head / x 
            | None -> 
                let possibleBigger = tail |> List.tryFind(fun a -> a > head && a % head = 0)
                match possibleBigger with
                | Some x -> x / head
                | None -> rowDivision tail 
        | [] -> failwithf "Unable to find divisors for row %A" row

    let computeSumOfDivision lines = 
        lines
        |> parseLines
        |> Array.map Array.toList
        |> Array.sumBy rowDivision

    let runDay2() = 
        let lines = File.ReadAllLines "Day2.txt"

        lines
        |> computeChecksum
        |> Console.WriteLine

        lines 
        |> computeSumOfDivision 
        |> Console.WriteLine    
