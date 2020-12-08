@bs.val external dirName: string = "__dirname"

let inputPath = Node.Path.resolve(dirName, "input.txt")

let rows = Util.parseRows(~path=inputPath)

let maxRowIndex = 127
let maxColumnIndex = 7

module Part1 = {
  type seat = {
    row: int,
    col: int,
    id: int,
  }

  type range = (int, int)

  type result = {rowRange: range, colRange: range, row: int, col: int}

  let delta = (a, b) => a - (a - b) / 2

  let main = () => {
    open Belt.Array

    let seats = rows->map(boardingPass => {
      let initial = {
        rowRange: (0, maxRowIndex),
        colRange: (0, maxColumnIndex),
        row: 0,
        col: 0,
      }

      let result = Util.explode(boardingPass)->reduceWithIndex(initial, (acc, x, i) => {
        let (leftRow, rightRow) = acc.rowRange
        let (leftCol, rightCol) = acc.colRange

        let rowDelta = delta(rightRow, leftRow)
        let colDelta = delta(rightCol, leftCol)

        switch (x, i) {
        // lower half (row)
        | ("F", 6) => {
            ...acc,
            row: min(leftRow, rightRow),
            rowRange: (leftRow, rowDelta),
          }
        | ("F", _) => {...acc, rowRange: (leftRow, rowDelta)}

        // upper half (row)
        | ("B", 6) => {
            ...acc,
            rowRange: (rowDelta, rightRow),
            row: max(leftRow, rightRow),
          }
        | ("B", _) => {...acc, rowRange: (rowDelta, rightRow)}

        // lower half (col)
        | ("L", 9) => {...acc, col: min(leftCol, rightCol)}
        | ("L", _) => {...acc, colRange: (leftCol, colDelta)}

        // upper half (col)
        | ("R", 9) => {...acc, col: max(leftCol, rightCol)}
        | ("R", _) => {...acc, colRange: (colDelta, rightCol)}

        // should never get here
        | _ => acc
        }
      })

      {row: result.row, col: result.col, id: result.row * 8 + result.col}
    })

    let maxId = seats->map(x => x.id)->reduce(0, max)

    Js.log(seats->keep(x => x.col <= 0 || x.row <= 0))

    Js.log2("Highest seat id:", maxId)
  }
}

module Part2 = {
  let main = () => {
    ()
  }
}

Util.Runner.run(~title="Binary Boarding", ~cases=[Part1.main, Part2.main])
