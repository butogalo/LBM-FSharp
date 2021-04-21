module LBM

(* Types and helpers *)
// The fluid will be spread on a grid, where each cell will store the amount of fluid going in the direction of its direct neighbouring cells separately.
// in the case of a 2D grid, each cell is connected to its neighbors by its direct borders and diagonals
type FluidAmountInOneDirection = float
// Array.length cell = 9 : (top, top-right, right... + center, the part of the fluid which doesnt move)
type Cell = FluidAmountInOneDirection array 
type Grid = Cell array array
// To know in which direction a certain amount of fluid particle is going.
type V2 = { x: float ; y: float }
          static member (*) (u, v) = u.x * v.x + u.y * v.y
          static member (*) (n, u) = { x = u.x * n ; y = u.y * n }
          static member (*) (u, n) = n * u
          static member (+) (u, v) = { x = u.x + v.x ; y = u.y + v.y }
          static member Zero       = { x = 0. ; y = 0. }
let V2 x y = { x = x ; y = y}
let map3D f cells = // helper maping function to be more concise
  cells  |> Array.Parallel.mapi (fun y column -> 
  column |> Array.Parallel.mapi (fun x cell   ->
  cell   |> Array.Parallel.mapi (fun i d      -> f x y i cell d)))  
let inside (x, y) cells = y > 0 && y < (Array.length cells) - 1 &&
                          x > 0 && x < (Array.length (cells.[y])) - 1

(* Core Logic *)
// each cell will use these 9 directions (top, top-right, right... + center) to spread its accordingly.
// cell.[1] represents the upward (V2 0. 1.) moving part of its fluid particles, and will thus use direction.[1], etc.
let directions = [| V2 0. 0. ; V2 0. 1. ; V2 1. 1. ; V2 1. 0. ; V2 1. -1. ; V2 0. -1. ; V2 -1. -1. ; V2 -1. 0. ; V2 -1. 1. |]

// a fluid amount going to the corners will see its spread weighted (multiplied by) by 1/36
// a fluid amount goint to the sides, by 1/9
// and one staying in the center, by 4/9
let weights = [|4./9.;1./9.;1./36.;1./9.;1./36.;1./9.;1./36.;1./9.;1./36.|]
// Array.sum weights = 1.
  
let density cell = Array.sum cell

// the overall horizontal and vertical velocity of a cell, given by the direction times the amount of fluid particles over the density of the whole cell
let velocity cell = (V2 (cell |> Array.mapi (fun i d ->  directions.[i].x * d) |> Array.sum)
                        (cell |> Array.mapi (fun i d ->  directions.[i].y * d) |> Array.sum))
                    * (1. / density cell)

let velocity' cell = (V2 (cell |> Array.mapi (fun i d ->  directions.[i].x * d) |> Array.sum)
                         (cell |> Array.mapi (fun i d ->  directions.[i].y * d) |> Array.sum))

// each moving subpart of a cell tends towards an equilibrium in relation to the whole cell, because of the collision between the fluid molecules
// for example, an droplet of water tends to stabilize as a stable rounder drop.
let equilibrium cell i =
  let c = 331.3 + 0.606 * 18. in let ω = weights.[i] // c = sound speed in 18C air
  let u = velocity cell       in let ϱ = density cell
  let e = directions.[i]
  ω * ϱ * (1. + (3. * e * u) / (c**2.) + (9.*((e * u)**2.)) / (2. * (c**4.)) - (3.*(u * u)) / (2. * (c**2.)))

// understanding opposite as if the indexes were on a circle, 1 at the top, clockwise, and 0 at the center, as we are talking about directions
let opposite_index i = [|0;5;6;7;8;1;2;3;4|].[i]

let simulate relaxation_parameter (obstacles: bool array array) cells = // relaxation_parameter in [0.5..2] 
  cells
  |> map3D (fun x y i cell density_of_particles_going_in_one_direction ->
                  if inside (x, y) cells
                  then // I. Stream, i.e., spread the fluid in all directions
                       // here, as we focus on each cell to compute its new value, we 'pump' (the streaming process) a certain amount (weighted by the equilibrium function, the collision process) of the fluid that would come towards it
                       // (rather than doing the opposite by moving the fluid to its neighbouring cells)
                       let y' = y + int (directions.[ opposite_index i ]).y
                       let x' = x + int (directions.[ opposite_index i ]).x
                       let cell' = cells.[y'].[x']
                       let f'    = cells.[y'].[x'].[i]
                       // II. Collide
                       if obstacles.[y'].[x']
                       then cell.[ opposite_index i ] // make the fluid bounce in the opposite direction if it meets an obstacle
                       else f' - (1./relaxation_parameter) * (f' - equilibrium cell' i) // we want the fluid to relax to its equilibrium state over time and not instantly, hence 1/relaxation_parameter as a factor of the equilibrium
                  else density_of_particles_going_in_one_direction)
