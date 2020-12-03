@bs.val external dirName: string = "__dirname"

let findMatch = (entries: array<int>, x: int, i: int): option<int> => {
  entries->Belt.Array.reduceWithIndex(None, (acc, y, j) => {
    switch (acc) {
      | None when i !== j && x + y === 2020 => Some(y)
      | _ => acc
    }
  })
}

let main = () => {
  let inputPath = Node.Path.resolve(dirName, "input.txt")
  let entries = Util.parseRows(inputPath)->Belt.Array.map(int_of_string)
  
  let maybePair = entries->Belt.Array.reduceWithIndex(None, (acc, x, i) => {
    switch (entries->findMatch(x, i)) {
      | Some(y) => Some([x, y])
      | _ => acc
    }
  })

  switch(maybePair) {
    | Some([x, y]) => Js.log2("Result: ", x * y)
    | _ => Js.log("Result: no match")
  }
}

main()