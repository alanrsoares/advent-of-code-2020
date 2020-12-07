@bs.val external dirName: string = "__dirname"

let findMatches = (entries: array<int>, target: int): option<array<int>> => {
  let result = ref(None)

  entries->Belt.Array.forEach((a) => {
    entries->Belt.Array.forEach((b) => {
      entries->Belt.Array.forEach((c) => {
        let sum = a + b + c

        result := switch result.contents {
          | None when (sum === target) => Some([a, b, c])
          | _ => result.contents
        }
      })
    })
  })

  result.contents
}


let main = () => {
  let inputPath = Node.Path.resolve(dirName, "input.txt")
  let entries = Util.parseRows(~path=inputPath)->Belt.Array.map(int_of_string)
  
  switch entries->findMatches(2020) {
    | Some([a, b, c]) => Js.log2("Result:", a * b * c)
    | _ => Js.log("Result: not found")
  }
}
