@bs.val external dirName: string = "__dirname"

let main = () => {
  let scheme: Toboggan_Trajectory.scheme = { 
    right: 3, 
    down: 1 
  }
  
  let trees = Toboggan_Trajectory.countTreesWithScheme(Toboggan_Trajectory.entries, scheme)

  Js.log(`Result: ${trees->string_of_int} trees`)
}