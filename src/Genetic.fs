module Genetic

open LBM

let random = System.Random()

let traverse f cells =
    cells  |> Array.Parallel.mapi (fun y column -> 
    column |> Array.Parallel.mapi (fun x cell   -> f x y cell))
  

let rec repeat f state count =
    if count = 0
    then f state 
    else repeat f (f state) (count - 1)


let fitness (shape: bool array array) cells (dna: bool array array) =
    let covers_shape =
         shape
        |> traverse (fun x y isObstacle -> if isObstacle then isObstacle && dna.[y].[x] else true)
        |> Array.concat
        |> Array.reduce (&&)
    
    let totalHorizontalVelocity =
                if covers_shape
                then repeat (simulate 1.2 dna) cells (60) 
                     |> Array.concat
                     |> Array.Parallel.map velocity'
                     |> Array.sum 
                     |> fun v -> v.x
                else 0.001
    printfn "%A" totalHorizontalVelocity
    if totalHorizontalVelocity < 0. then 0.002 else totalHorizontalVelocity


let crossover ((dna1: bool array array), (dna2: bool array array)) =
    let splitIndexY = random.Next (Array.length dna1)
    let splitIndexX = random.Next (Array.length dna1.[0])
    dna1 |> traverse (fun x y cell -> if y <= splitIndexY || x <= splitIndexX then dna1.[y].[x] else dna2.[y].[x])


let weighted_choice (weighted: (float * 'a) array) =
    let total = Array.sumBy fst weighted
    let threshold = random.NextDouble() * total
    let weightedSoFar = Array.mapFold (fun acc (weight, specimen) -> ((acc - weight, specimen), acc - weight)) threshold weighted
    // printfn "%A" (Array.map fst (fst weightedSoFar))
    Array.findBack (fun (accumulated_weight,_) -> accumulated_weight < 0.) (fst weightedSoFar)


let make_couple specimens = 
    let (w, dna1) = weighted_choice specimens
    let (_, dna2) = Array.except [w,dna1] specimens
                    |> weighted_choice
    dna1, dna2

    
let sexualReproduction specimens =
    let elite = Array.take 3 specimens
    let newGen = [| for i in 0..10 -> make_couple specimens |> crossover |]
    Array.append (elite |> Array.map snd) newGen

    
let mutate rate (shape: bool array array) (dna: bool array array) =
    dna
    |> traverse (fun x y bit ->
        if inside (x,y) dna
        then let shellAround = 
                    let x = Array.countBy id (Array.map (fun v -> dna.[y + int v.y].[x + int v.x]) directions)
                            |> Array.tryFind (fst >> not)
                    match x with
                    | Some (_, n) -> n >= 2
                    | _ -> false
             let airAround   = Array.reduce (||) (Array.map (fun v -> not dna.[y + int v.y].[x + int v.x]) directions)
             if shellAround && airAround && random.NextDouble() < rate && not shape.[y].[x] 
             then not bit
             else bit
        else bit) 


let evolve shape_to_cover cells mutation_rate (mostFitScores, specimens) =
    let newGen = 
        specimens
        |> Array.Parallel.map (mutate mutation_rate shape_to_cover)
        |> Array.Parallel.map (fun specimen -> let f = fitness shape_to_cover cells specimen in
                                               f, specimen)
        |> Array.sortByDescending fst
        |> Array.take 10
    let mostFitScores' = fst newGen.[0] :: mostFitScores
    printfn "%A" mostFitScores'
    mostFitScores', sexualReproduction newGen
