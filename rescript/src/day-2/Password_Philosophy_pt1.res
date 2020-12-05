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
  min: int,
  max: int,
  character: char,
  pwd: string
}

let parseEntry = (s: string) => {
  let (left, right) = Util.bissect(':', s)
  let (rule, character) = Util.bissect(' ', left)
  let (min, max) = Util.bissectMap('-', rule, int_of_string)

  {
   min,
   max,
   character: character->String.get(0),
   pwd: right->String.trim
  }
}

let countOccurrences = (str, ch)=> {
  let count = ref(0)

  str |> String.iter(x => if (x === ch) { count := count.contents + 1 })

  count.contents
}

let isValidEntry = (entry) => {
  let { min, max, character, pwd } = entry
  
  Util.isBetween(
    ~value=pwd->countOccurrences(character), 
    ~min, 
    ~max
  )
}

let main = () => {
  let inputPath = Node.Path.resolve(dirName, "input.txt")
  let entries = Util.parseRows(inputPath)->Belt.Array.map(parseEntry)
  
  let validEntries = entries->Belt.Array.keep(isValidEntry)
  let result = validEntries->Belt.Array.length

  result |> Js.log2("Valid passwords found:")
}