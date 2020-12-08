@bs.val external dirName: string = "__dirname"

let inputPath = Node.Path.resolve(dirName, "input.txt")

let rows = Util.parseRows(~path=inputPath)

let maxRowIndex = 127
let maxColumnIndex = 7

type seat = {
  row: int,
  col: int,
  id: int,
}

let delta = (a, b) => a - (a - b) / 2

let isOneOf = (xs, x) => Js.Array.includes(x, xs)

type direction = Lower | Upper

let toDirection = x => {
  switch x {
  | "L" | "F" => Lower
  | "R" | "B" => Upper
  | _ => Upper
  }
}

let bissectWithBounds = (steps: array<direction>, treeBounds) => {
  open Belt.Array

  let last = steps->getUnsafe(steps->length - 1)

  let (left, right) = steps->reduce(treeBounds, ((l, r), step) => {
    let delta' = delta(r, l)

    switch step {
    | Lower => (l, delta')
    | Upper => (delta', r)
    }
  })

  let settle = switch last {
  | Lower => min
  | Upper => max
  }

  settle(left, right)
}

module Part1 = {
  let main = () => {
    open Belt.Array

    let seats = rows->map(boardingPass => {
      let steps = Util.explode(boardingPass)

      let (rowSteps, colSteps) = (
        steps->keep(isOneOf(["F", "B"]))->map(toDirection),
        steps->keep(isOneOf(["L", "R"]))->map(toDirection),
      )

      let (row, col) = (
        rowSteps->bissectWithBounds((0, maxRowIndex)),
        colSteps->bissectWithBounds((0, maxColumnIndex)),
      )

      {
        row: row,
        col: col,
        id: row * 8 + col,
      }
    })

    let maxId = seats->map(x => x.id)->reduce(0, max)

    Js.log2("Highest seat id:", maxId)
  }
}

module Part2 = {
  let main = () => {
    ()
  }
}

Util.Runner.run(~title="Binary Boarding", ~cases=[Part1.main, Part2.main])
