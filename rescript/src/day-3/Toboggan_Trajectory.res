@bs.val external dirName: string = "__dirname"

type strategy = {
  right: int,
  down: int,
}

type position = {
  row: int,
  column: int,
}

let parseEntry = entry => {
  let result = Belt.Array.make(entry->String.length, '-')

  entry->String.iteri((i, char) => {result[i] = char}, _)

  result
}

let inputPath = Node.Path.resolve(dirName, "input.txt")
let entries = Util.parseRows(~path=inputPath)->Belt.Array.map(parseEntry)

let countTreesWithStrategy = (rows, scheme) => {
  let count = ref(0)
  let position = ref({row: 0, column: 0})
  let break = ref(false)

  let rowsLength = rows->Belt.Array.length

  while !break.contents {
    // move
    let {row, column} = position.contents
    let (row', column') = (row + scheme.down, column + scheme.right)

    position := {row: row', column: column'}

    break := row' > rowsLength - 1

    if !break.contents {
      let row = rows[row']
      let rowLength = row->Belt.Array.length
      let safeX = mod(column', rowLength)
      let tile = row[safeX]

      if tile === '#' {
        count := count.contents + 1
      }
    }
  }

  count.contents
}

module Part1 = {
  let main = () => {
    let strategy = {
      right: 3,
      down: 1,
    }

    let trees = entries->countTreesWithStrategy(strategy)

    Js.log2(`Trees encountered:`, trees)
  }
}

module Part2 = {
  let strategies = [
    {right: 1, down: 1},
    {right: 3, down: 1},
    {right: 5, down: 1},
    {right: 7, down: 1},
    {right: 1, down: 2},
  ]

  let main = () => {
    open Belt.Array

    let trees = strategies->map(entries->countTreesWithStrategy)->map(float_of_int)->Util.product

    Js.log2(`Trees encountered:`, trees)
  }
}

Util.Runner.run(~title="Toboggan Trajectory", ~cases=[Part1.main, Part2.main])
