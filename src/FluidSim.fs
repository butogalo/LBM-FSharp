open System
open Raylib_cs
open LBM



let mutable bike = new System.Drawing.Bitmap "bike_100width_monochrome.bmp"
let bmp = [| for y in 0..bike.Height - 1 ->
             [| for x in 0..bike.Width - 1 ->
                if (bike.GetPixel (x, y)).GetBrightness() < 0.5f
                then true
                else false |] |]      



let random = Random()
let grey g = Color (byte g, byte g, byte g, byte 0)


module Init =
  let initCells width height =
    [|for y in 1..height -> [|for x in 1..width -> [|for i in 1..9 -> 0.01 * random.NextDouble() |]|]|]

  let initObstacles width height =
    [|for y in 0..height ->
      [|for x in 0..width ->   x = 0
                            || y = 0
                            // || x = width - 1
                            || y = height - 1
                            |]|]

  let cylinder (pos_x, pos_y) diameter x y = (float x - pos_x)**2. + (float y - pos_y)**2. < diameter
  let box (pos_x, pos_y) scale x y = x > pos_x && x < pos_x + scale &&  y > pos_y && y <  pos_y +  scale 
  let ellipse (pos_x, pos_y) width height x y = (float (x - pos_x))**2. / (width ** 2.) + (float (y - pos_y))**2. / (height ** 2.) <= 1.
  
  let createObstacle obstacle obstacles =
    obstacles |> Array.Parallel.mapi (fun y column ->
    column    |> Array.Parallel.mapi (fun x _      -> obstacle x y || obstacles.[y].[x])) 
  



module View =
  // map an interval to another one
  let map n (start1, stop1) (start2, stop2) = ((n-start1)/(stop1-start1)) * (stop2-start2) + start2

  let viewV cells (obstacles: bool array array) scale =
    cells  |> Array.iteri (fun y column ->
    column |> Array.iteri (fun x cell  ->
      let color = if (not (obstacles.[y].[x]))
                  then (map (float32 <| density cell) (10.f, 0.f) (0.f, 100.f), 1f, 1f)
                  else (0f, 0f, 0f)
                  |> Raylib.ColorFromHSV
      let v' = velocity' cell
      let (x_start, y_start) = x*scale, y*scale
      let (x', y') = x_start + int (100.*v'.x), y_start + int (100.*v'.y)
      Raylib.DrawLine (x_start, y_start, x', y', color)
      Raylib.DrawRectangle(x', y', 2, 2, color)
    )) 

  let viewImage obstacles (shape: bool array array) scale = 
    obstacles |> Array.iteri (fun y column ->
    column    |> Array.iteri (fun x cell  ->
      let color = if shape.[y].[x] && obstacles.[y].[x]
                  then Color.BLACK
                  else if obstacles.[y].[x]
                  then Color.GREEN
                  else if shape.[y].[x]
                  then Color.RED
                  else Color.WHITE
      Raylib.DrawRectangle(x * scale, y * scale, scale, scale, color)
    ))

  let view cells shape obstacles scale =
    if Raylib.IsKeyDown KeyboardKey.KEY_SPACE
    then viewImage obstacles shape scale
    else viewV cells obstacles scale


[<EntryPoint>]
let main argv =
    let fan i strength cells =
      let uLB = 0.04
      cells |> map3D (fun x y i cell d -> if x = 2 && i = i then strength else d)
    
    let averageAround rho0 cells =
      cells |> map3D (fun x y i cell d -> d * (rho0 / density cells.[y].[x]))


    Raylib.InitWindow (1000, 800, "Fluid Sim") 
    //Raylib.SetTargetFPS 120

    let WIDTH  = bike.Width
    let HEIGHT = bike.Height
    let SCALE  = 5
    let mutable i = 3
    let mutable strength = 1.
    let mutable tau = 1.2

    // the grid
    let mutable cells = Init.initCells WIDTH HEIGHT
    
    // Uncomment to run the genetic algorithm
    //let gen0 = [| for i in 0..10 -> Genetic.mutate 0.0005 bmp bmp |] 
    //let evolved_design = Genetic.repeat (Genetic.evolve bmp cells 0.0005) ([], gen0) 3
                          


    let obstacles = bmp //(snd evolved_design).[0]

    // save the evolved design of the bike
    // Array.iteri (fun y c -> Array.iteri (fun x b -> bike.SetPixel(x, y, if b then System.Drawing.Color.RoyalBlue else System.Drawing.Color.White)) c ) obstacles
    // bike.Save "newBike.bmp"
 

    while not (Raylib.WindowShouldClose ()) do
        cells <- simulate tau obstacles cells
                 |> fan i strength
                 
        
        if Raylib.IsKeyPressed KeyboardKey.KEY_R
        then cells <- Init.initCells WIDTH HEIGHT       
        else ()

        i <- if Raylib.IsKeyReleased KeyboardKey.KEY_RIGHT
                     then min 8 (i + 1)
             else if Raylib.IsKeyReleased KeyboardKey.KEY_LEFT
                     then max 0 (i - 1)
                     else i
        tau <- if Raylib.IsKeyReleased KeyboardKey.KEY_A
               then tau - 0.1
               else if Raylib.IsKeyReleased KeyboardKey.KEY_D
               then tau + 0.1
               else tau
        strength <- if Raylib.IsKeyReleased KeyboardKey.KEY_Q
                    then strength - 0.1
                    else if Raylib.IsKeyReleased KeyboardKey.KEY_E
                    then strength + 0.1
                    else strength


        let (x, y) = float (Raylib.GetMouseX()) / float SCALE |> floor |> int ,
                     float (Raylib.GetMouseY()) / float SCALE |> floor |> int 
        if inside (x, y) cells
        then if Raylib.IsMouseButtonDown MouseButton.MOUSE_LEFT_BUTTON
             then cells.[y].[x].[i] <- 2.
             else if Raylib.IsMouseButtonDown MouseButton.MOUSE_RIGHT_BUTTON
             then obstacles.[y].[x] <- not obstacles.[y].[x]
             else ()
        else ()

        if inside(x, y) cells
        then Raylib.DrawText (sprintf "vx %f\nvy %f\nrho %f" (velocity cells.[y].[x]).x (velocity cells.[y].[x]).y (density cells.[y].[x]), 650, 50, 20, Color.ORANGE)
        else ()

        Raylib.BeginDrawing ()
        Raylib.ClearBackground (grey 50)
       
        View.view cells bmp obstacles SCALE
        
        Raylib.DrawFPS (650, 10)
        Raylib.DrawText (string i, 650, 30, 20, Color.GREEN)
        Raylib.DrawText (string tau, 700, 30, 20, Color.GREEN)
        Raylib.DrawText (string strength, 750, 30, 20, Color.GREEN)




        Raylib.EndDrawing ()
    Raylib.CloseWindow()

    0 // return an integer exit code
