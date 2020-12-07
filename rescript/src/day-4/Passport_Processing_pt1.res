
let run = () => {
  open Passport_Processing

  let validPassports = rows
    ->normalizeRows
    ->Belt.Array.keep(isValid)
    ->Belt.Array.length

  Js.log2(`Valid passports:`, validPassports)
}