@bs.val external dirName: string = "__dirname"

let schemes: array<Toboggan_Trajectory.scheme> = [
  { right: 1, down: 1 },
  { right: 3, down: 1 },
  { right: 5, down: 1 },
  { right: 7, down: 1 },
  { right: 1, down: 2 }
]

let main = () => {
  open Toboggan_Trajectory
  open Belt.Array

  let runWithScheme = scheme => entries->countTreesWithScheme(scheme)
  
  let trees = schemes->map(runWithScheme)->map(float_of_int)->Util.product

  Js.log(`Result: ${trees->Js.Float.toString} trees`)
}