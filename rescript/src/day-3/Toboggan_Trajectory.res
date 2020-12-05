@bs.val external dirName: string = "__dirname"

type strategy = {
  right: int,
  down: int
}

type position = {
  row: int,
  column: int
}

let parseEntry = (entry) => {
  let result = Belt.Array.make(entry->String.length, '-')
  entry |> String.iteri((i, char) => { result[i] = char })
  
  result
}

let inputPath = Node.Path.resolve(dirName, "input.txt")
let entries = Util.parseRows(inputPath)->Belt.Array.map(parseEntry)

let countTreesWithStrategy = (rows, scheme) => {
  let count = ref(0)
  let position = ref({ row: 0, column: 0 })
  let break = ref(false)

  let rowsLength = rows->Belt.Array.length

  while !break.contents {
    // move
    let { row, column } = position.contents
    let (row', column') = (row + scheme.down, column + scheme.right)
    
    position := { row: row', column: column' }

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