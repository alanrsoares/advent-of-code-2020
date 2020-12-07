@bs.val external dirName: string = "__dirname"

let inputPath = Node.Path.resolve(dirName, "input.txt")
let entries = Util.parseRows(~path=inputPath)->Belt.Array.map(int_of_string)

module Pt1 = {
  let findMatches = (entries: array<int>, target: int): option<array<int>> => {
    open Belt.Array

    let result = ref(None)

    entries->forEach(a => {
      entries->forEach(b => {
        let sum = a + b

        result :=
          switch result.contents {
          | None when sum === target => Some([a, b])
          | _ => result.contents
          }
      })
    })

    result.contents
  }

  let run = () => {
    switch entries->findMatches(2020) {
    | Some([a, b]) => Js.log2("Result:", a * b)
    | _ => Js.log("Result: not found")
    }
  }
}

module Pt2 = {
  let findMatches = (entries: array<int>, target: int): option<array<int>> => {
    open Belt.Array

    let result = ref(None)

    entries->forEach(a => {
      entries->forEach(b => {
        entries->forEach(c => {
          let sum = a + b + c

          result :=
            switch result.contents {
            | None when sum === target => Some([a, b, c])
            | _ => result.contents
            }
        })
      })
    })

    result.contents
  }

  let run = () => {
    switch entries->findMatches(2020) {
    | Some([a, b, c]) => Js.log2("Result:", a * b * c)
    | _ => Js.log("Result: not found")
    }
  }
}

Util.Runner.run(~title="Report Repair", ~cases=[Pt1.run, Pt2.run])
