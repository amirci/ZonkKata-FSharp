namespace ZonkKata

module Roll =
    let SingleDiePoints = function | 1 -> 100 | 5 -> 50 | _ -> 0

    let SumOnesAndFives = Seq.map SingleDiePoints >> Seq.sum

    let ThreePairsPoints = 750

    let ThreeOfAKindPoints = function | 1 -> 1000 | n -> n * 100

    let XOfAKindPoints (dice, repeat) = (repeat - 2) * (dice |> ThreeOfAKindPoints)

    let FourOfAKindPoints n = (n, 4) |> XOfAKindPoints

    let FiveOfAKindPoints n = (n, 5) |> XOfAKindPoints

    let SixOfAKindPoints n = (n, 6) |> XOfAKindPoints

    let (|ThreePairs|_|) =
        function
        | [(_, 2); (_, 2); (_, 2)]   -> Some ThreePairs
        | [(2,4); (_,2)]             -> Some ThreePairs
        | [(3,4); (x,2)] when x <> 1 -> Some ThreePairs
        | _ -> None

    let (|GroupPoints|_|) roll = 
        let sumPoints =
            let calcPoints (x, count) =
                match count with 
                | _ when count < 3 -> count * (x |> SingleDiePoints)
                | _                -> (x, count) |> XOfAKindPoints
            
            List.map calcPoints >> List.sum

        let groupAndCount = 
            let byFreq (x, s) = x, s|> Seq.length
            Seq.groupBy id
            >> Seq.map byFreq
            >> Seq.sortBy snd
            >> Seq.toList
            >> List.rev

        roll 
        |> groupAndCount
        |> function
        | ThreePairs -> ThreePairsPoints
        | byCount    -> byCount |> sumPoints
        |> Some

    let CalculatePoints =
        List.sort
        >> function
           | [1; 2; 3; 4; 5; 6] -> 1000
           | GroupPoints pts    -> pts
           | sorted             -> sorted |> SumOnesAndFives

    let PrintPoints = 
      CalculatePoints 
      >> function
         | 0 -> printfn "Zonk!"
         | p -> printfn "You rolled %i points." p

