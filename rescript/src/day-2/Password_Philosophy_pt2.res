@bs.val external dirName: string = "__dirname"

let findMatches = (entries: array<int>, target: int): option<array<int>> => {
  let result = ref(None)

  entries->Belt.Array.forEach((a) => {
    entries->Belt.Array.forEach((b) => {
      let sum = a + b

      result := switch result.contents {
        | None when sum === target => Some([a, b])
        | _ => result.contents
      }
    })
  })

  result.contents
}

type entry = {
  pos1: int,
  pos2: int,
  character: char,
  pwd: string
}

let parseEntry = (s: string) => {
  let (left, right) = Util.bissect(':', s)
  let (rule, character) = Util.bissect(' ', left)
  let (pos1, pos2) = Util.bissectMap('-', rule, int_of_string)

  {
   pos1,
   pos2,
   character: character->String.get(0),
   pwd: right->String.trim
  }
}

let isValidEntry = (entry) => {
  let { pos1, pos2, character, pwd } = entry
  let isExpectedCharacter = x => pwd->String.get(x - 1) === character
  
  [pos1, pos2]
    ->Belt.Array.keep(isExpectedCharacter)
    ->Belt.Array.length === 1
}

let main = () => {
  let inputPath = Node.Path.resolve(dirName, "input.txt")
  let entries = Util.parseRows(inputPath)->Belt.Array.map(parseEntry)
  
  let validEntries = entries->Belt.Array.keep(isValidEntry)
  let result = validEntries->Belt.Array.length

  result |> Js.log2("Valid passwords found:")
}