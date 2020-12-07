@bs.val external dirName: string = "__dirname"

let inputPath = Node.Path.resolve(dirName, "input.txt")

type passport = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: string
}


let requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

let isValid = (fields: array<(string, string)>) => {
  open Belt.Array

  let isMissing = (key: string) => !(fields->some(((k, _)) => (k === key)))
  let missingFields = requiredKeys->keep(isMissing)

  // sneaky backdoor
  missingFields->joinWith("", x => x) === "cid" ||
  (fields->length === requiredKeys->length && missingFields->length === 0)
}

let line_break = "#<br />"

let parseRow = (row: string) => {
  open Belt.Array

  row
    ->String.trim
    ->Util.splitToArray(' ', _)
    ->keepMap(kv => {
      switch kv->Util.splitToArray(':', _) {
      | [k, v] => Some((k, v))
      | _ => None
      }
    })
}

let normalizeRows = (rows: array<string>) => {
  open Belt.Array
  
  rows
    ->map(row => ((row->String.trim !== "") ? row : line_break))
    ->joinWith(" ", x => x)
    ->Js.String.split(line_break, _)
    ->map(parseRow)
}

let rows = Util.parseRows(~path=inputPath)


module Pt1 = {
  let run = () => {
    let validPassports = rows
      ->normalizeRows
      ->Belt.Array.keep(isValid)
      ->Belt.Array.length

    Js.log2(`Valid passports:`, validPassports)
  }
}

module Pt2 = {
  let run = () => {
    let validPassports = rows
      ->normalizeRows
      ->Belt.Array.keep(isValid)
      ->Belt.Array.length

    Js.log2(`Valid passports:`, validPassports)
  }
}

Util.Runner.run(
  ~title="Passport Processing", 
  ~cases=[
    Pt1.run,
    Pt2.run,
  ]
)