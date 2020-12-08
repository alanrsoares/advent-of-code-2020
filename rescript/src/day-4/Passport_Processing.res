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

  row->String.trim->Util.splitToArray(' ', _)->map(Util.bisect(':', _))
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
  open Belt.Array

  let main = () => {
    let validPassports = rows->normalizeRows->keep(hasAllRequiredFields)->length

    Js.log2(`Valid passports:`, validPassports)
  }
}

module Validation = {
  open Belt.Array

  let applyRules = (rules, s: string) => rules->every(r => r(s))

  let hasLength = (s, n) => s->String.length === n
  let isBetween = (s, min, max) => Util.isBetween(~value=s->int_of_string, ~min, ~max)
  let isDigit = s => s->Js.Re.test_(%re("/^\d+$/g"), _)
  let isDigitWithLength = (s, n) => s->isDigit && s->hasLength(n)
  let isValidHeight = s => {
    let maybeResult = s->Js.Re.exec_(%re("/^(\d+)(cm|in)$/g"), _)
    maybeResult->Belt.Option.mapWithDefault(false, result => {
      let captured = result->Js.Re.captures->map(Js.Nullable.toOption)->sliceToEnd(1)

      switch captured {
      | [Some(v), Some("cm")] => v->isDigit && v->isBetween(150, 193)
      | [Some(v), Some("in")] => v->isDigit && v->isBetween(59, 76)
      | _ => false
      }
    })
  }
  let isHexColor = s => s->Js.Re.test_(%re("/^#(([a-f0-9]){3}|([a-f0-9]{6}))$/ig"), _)
  let isEyeColor = s =>
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]->some(color => color === s)
}

module Part2 = {
  open Belt.Array
  open Validation

  let rulesMap =
    [
      ("byr", [isDigitWithLength(_, 4), isBetween(_, 1920, 2002)]->applyRules),
      ("iyr", [isDigitWithLength(_, 4), isBetween(_, 2010, 2020)]->applyRules),
      ("eyr", [isDigitWithLength(_, 4), isBetween(_, 2020, 2030)]->applyRules),
      ("hgt", isValidHeight),
      ("hcl", isHexColor),
      ("ecl", isEyeColor),
      ("pid", isDigitWithLength(_, 9)),
      ("cid", _ => true),
    ]->Belt.Map.String.fromArray

  let main = () => {
    let withValidFields =
      rows->normalizeRows->keep(hasAllRequiredFields)->map(Belt.Map.String.fromArray)

    let isValidPassport = Belt.Map.String.every(_, (key, value) => {
      switch rulesMap->Belt.Map.String.get(key) {
      | Some(rule) => rule(value)
      | _ => false
      }
    })

    let validPassports = withValidFields->keep(isValidPassport)

    Js.log2(`Valid passports:`, validPassports->length)
  }
}

Util.Runner.run(~title="Passport Processing", ~cases=[Part1.main, Part2.main])
