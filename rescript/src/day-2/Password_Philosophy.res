@bs.val external dirName: string = "__dirname"

let inputPath = Node.Path.resolve(dirName, "input.txt")

module Part1 = {
  type entry = {
    min: int,
    max: int,
    character: char,
    pwd: string,
  }

  let parseEntry = (s: string) => {
    let (left, right) = Util.bisect(':', s)
    let (rule, character) = Util.bisect(' ', left)
    let (min, max) = Util.bisectMap('-', rule, int_of_string)

    {
      min: min,
      max: max,
      character: character->String.get(0),
      pwd: right->String.trim,
    }
  }

  let countOccurrences = (str, ch) => {
    let count = ref(0)

    str->String.iter(x =>
      if x === ch {
        count := count.contents + 1
      }
    , _)

    count.contents
  }

  let isValidEntry = entry => {
    let {min, max, character, pwd} = entry

    Util.isBetween(~value=pwd->countOccurrences(character), ~min, ~max)
  }

  let main = () => {
    open Belt.Array

    let entries = Util.parseRows(~path=inputPath)->map(parseEntry)
    let validEntries = entries->keep(isValidEntry)
    let result = validEntries->length

    result->Js.log2("Valid passwords found:", _)
  }
}

module Part2 = {
  type entry = {
    pos1: int,
    pos2: int,
    character: char,
    pwd: string,
  }

  let parseEntry = (s: string) => {
    let (left, right) = Util.bisect(':', s)
    let (rule, character) = Util.bisect(' ', left)
    let (pos1, pos2) = Util.bisectMap('-', rule, int_of_string)

    {
      pos1: pos1,
      pos2: pos2,
      character: character->String.get(0),
      pwd: right->String.trim,
    }
  }

  let isValidEntry = entry => {
    open Belt.Array

    let {pos1, pos2, character, pwd} = entry
    let isExpectedCharacter = x => pwd->String.get(x - 1) === character

    [pos1, pos2]->keep(isExpectedCharacter)->length === 1
  }

  let main = () => {
    open Belt.Array

    let inputPath = Node.Path.resolve(dirName, "input.txt")
    let entries = Util.parseRows(~path=inputPath)->map(parseEntry)

    let validEntries = entries->keep(isValidEntry)
    let result = validEntries->length

    result->Js.log2("Valid passwords found:", _)
  }
}

Util.Runner.run(~title="Password Philosophy", ~cases=[Part1.main, Part2.main])
