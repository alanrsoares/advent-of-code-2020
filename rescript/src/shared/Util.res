// plumbing

let splitToArray = (c, s) => String.split_on_char(c, s)->Belt.List.toArray

let parseRows = (~path) => {
  let file = Node.Fs.readFileAsUtf8Sync(path)

  splitToArray('\n', file)
}

let sum = Belt.Array.reduce(_, 0, (a, b) => a + b)

let product = Belt.Array.reduce(_, 1.0, (a, b) => a *. b)

exception String_cannot_be_split

let bisect = (c, s) => {
  switch splitToArray(c, s) {
  | [l, r] => (l, r)
  | _ => raise(String_cannot_be_split)
  }
}

let bisectMap = (c, s, fn) => {
  switch splitToArray(c, s)->Belt.Array.map(fn) {
  | [l, r] => (l, r)
  | _ => raise(String_cannot_be_split)
  }
}

let explode = Js.String.split("", _)

let isBetween = (~value: int, ~min, ~max) => value >= min && value <= max

module Runner = {
  let run = (~title, ~cases) => {
    cases->Belt.Array.forEachWithIndex((i, fn) => {
      Js.log(`Running "${title}" part ${(i + 1)->string_of_int}`)
      fn()
    })
  }
}
