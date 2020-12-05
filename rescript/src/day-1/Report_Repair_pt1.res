@bs.val external dirName: string = "__dirname"

let findMatches = (entries: array<int>, target: int): option<array<int>> => {
  let result = ref(None)

  entries->Belt.Array.forEach((a) => {
    entries->Belt.Array.forEach((b) => {
      let sum = a + b

      result := switch result.contents {
        | None when (sum === target) => Some([a, b])
        | _ => result.contents
      }
    })
  })

  result.contents
}

let main = () => {
  let inputPath = Node.Path.resolve(dirName, "input.txt")
  let entries = Util.parseRows(inputPath)->Belt.Array.map(int_of_string)
  
  switch entries->findMatches(2020) {
    | Some([a, b]) => Js.log2("Result:", a * b)
    | _ => Js.log("Result: not found")
  }
}
