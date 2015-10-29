namespace ZonkKata

module Roll =
    let SingleDicePoints = function | 1 -> 100 | 5 -> 50 | _ -> 0

    let SumOnesAndFives = Seq.map SingleDicePoints >> Seq.sum

    let ThreePairsPoints = 750

    let ThreeOfAKindPoints = function | 1 -> 1000 | n -> n * 100

    let XOfAKindPoints (dice, repeat) = (repeat - 2) * (dice |> ThreeOfAKindPoints)

    let FourOfAKindPoints repeat = (4, repeat) |> XOfAKindPoints

    let FiveOfAKindPoints repeat = (5, repeat) |> XOfAKindPoints

    let SixOfAKindPoints repeat = (6, repeat) |> XOfAKindPoints

    let (|GroupPoints|_|) roll = 
        let groupPoints (x, c) =
            match c with 
            | _ when c < 3 -> c * (x |> SingleDicePoints)
            | _            -> (x, c) |> XOfAKindPoints

        let byFreq (x, s) = x, s|> Seq.length

        roll 
        |> Seq.groupBy id
        |> Seq.map byFreq
        |> Seq.sortBy snd
        |> Seq.toList
        |> List.rev
        |> function
           | [(_,2); (_,2); (_,2)]      -> Some ThreePairsPoints
           | [(2,4); (_,2)]             -> Some ThreePairsPoints
           | [(3,4); (x,2)] when x <> 1 -> Some ThreePairsPoints
           | (g, c) :: t    when c >= 3 -> ((g, c)::t) |> List.map groupPoints |> List.sum |> Some
           | _ -> None

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

