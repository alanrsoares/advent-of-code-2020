@bs.val external dirName: string = "__dirname"

let inputPath = Node.Path.resolve(dirName, "input.txt")

let requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

let hasAllRequiredFields = (fields: array<(string, string)>) => {
  open Belt.Array

  let isMissing = (key: string) => !(fields->some(((k, _)) => k === key))
  let missingFields = requiredKeys->keep(isMissing)

  // sneaky backdoor
  missingFields == ["cid"] || fields->length === requiredKeys->length
}

let line_break = "#<br />"

let parseRow = (row: string) => {
  open Belt.Array

  row->String.trim->Util.splitToArray(' ', _)->map(Util.bissect(':', _))
}

let normalizeRows = (rows: array<string>) => {
  open Belt.Array

  rows
  ->map(row => row->String.trim !== "" ? row : line_break)
  ->joinWith(" ", x => x)
  ->Js.String.split(line_break, _)
  ->map(parseRow)
}

let rows = Util.parseRows(~path=inputPath)

module Part1 = {
  let main = () => {
    let validPassports =
      rows->normalizeRows->Belt.Array.keep(hasAllRequiredFields)->Belt.Array.length

    Js.log2(`Valid passports:`, validPassports)
  }
}

module Part2 = {
  let main = () => {
    let validPassports =
      rows->normalizeRows->Belt.Array.keep(hasAllRequiredFields)->Belt.Array.length

    Js.log2(`Valid passports:`, validPassports)
  }
}

Util.Runner.run(~title="Passport Processing", ~cases=[Part1.main, Part2.main])
