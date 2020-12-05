@bs.val external dirName: string = "__dirname"

let main = () => {
  open Toboggan_Trajectory

  let strategy = {
    right: 3, 
    down: 1 
  }

  let trees = countTreesWithStrategy(entries, strategy)

  Js.log(`Result: ${trees->string_of_int} trees`)
}