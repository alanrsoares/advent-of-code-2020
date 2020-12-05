@bs.val external dirName: string = "__dirname"

open Toboggan_Trajectory

let strategies = [
  { right: 1, down: 1 },
  { right: 3, down: 1 },
  { right: 5, down: 1 },
  { right: 7, down: 1 },
  { right: 1, down: 2 }
]

let main = () => {
  open Belt.Array

  let runWithStrategy = strategy => 
    entries->countTreesWithStrategy(strategy)
  
  let trees = strategies
    ->map(runWithStrategy)
    ->map(float_of_int)
    ->Util.product

  Js.log(`Result: ${trees->Js.Float.toString} trees`)
}