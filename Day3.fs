namespace AdventOfCode

open System
open System.Collections.Generic
open System.Globalization


module Day3 = 
    let bottomCornerStartValue k =
        let s = 1 + 4 * k * k + 4 * k + 1
        //printfn "Start value for k = %A: %A" k s
        s

    let findK number = 
        let k = 
            seq {0..number} // we can so totally optimize this. All I have to do is solve a deg 2 polynomial and take the larger solution. 
            |> Seq.tryFind(fun x -> bottomCornerStartValue x <= number && bottomCornerStartValue (x + 1) > number)

        if (k.IsSome) then 
            //printfn "K = %A for %A" k.Value number
            k.Value
        else
            failwithf "Unable to find k for %A" number

        
    
    let findIndexes number k = 
        let startValue = k |> bottomCornerStartValue

        let next = k + 1

        if (number >= startValue && number < startValue + 2 * next) then
            (startValue + 2 * next - number, 2*next + 1)
        elif (number >= startValue + 2 * next && number < startValue + 4 * next) then
            (1, startValue + 4 * next - number)
        elif (number >= startValue + 4 * next && number < startValue + 6 * next) then
            (startValue + 6 * next - number, 1)
        elif (number >= startValue + 6 * next && number < startValue + 8 * next) then 
            (2 * next + 1, number - startValue - 6 * next + 2)  
        else failwithf "incorrect algorithm for %A" number

    let manhattanDistanceToCenter (x, y) (center: int) =
        let dimension = [x; y] |> List.max
        let distance = Math.Abs(x - center) + Math.Abs(y - center)
        distance 

    let computeForNumber number = 
        let k = number |> findK 
        let center = k + 2 
        let indices = k |> findIndexes number
        let distance = center |> manhattanDistanceToCenter indices
        distance 


    let adjacentSum (matrix: int [] []) (x, y) = 
        matrix.[x-1].[y-1] + 
            matrix.[x-1].[y] + 
            matrix.[x-1].[y+1] + 
            matrix.[x].[y+1] + 
            matrix.[x].[y-1] + 
            matrix.[x+1].[y] + 
            matrix.[x+1].[y+1] + 
            matrix.[x+1].[y-1]


    let matrixComputation number = 
        let k = number |> findK
        let indices = k |> findIndexes number 
        let center = k + 1
        let matrix = Array.create (2 * center + 1) (Array.zeroCreate (2 * center + 1))
        matrix.[center].[center] <- 1
        
//        fun rec fillMatrix matrix position k center stopValue = 
            


        0 


    let runDay3() =
        25 
        |> computeForNumber
        |> printfn "%A for 25"

        12
        |> computeForNumber
        |> printfn "%A for 12"

        1024 
        |> computeForNumber
        |> printfn "%A for 1024"

        23 
        |> computeForNumber
        |> printfn "%A for 23"

        325489
        |> computeForNumber        
        |> printfn "%A for 325489"

