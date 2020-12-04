let splitToArray = (c, s) => String.split_on_char(c, s)->Belt.List.toArray;

let parseRows = (path) => {
    let file = Node.Fs.readFileAsUtf8Sync(path)

    splitToArray('\n', file)
}

let sum = (xs: array<int>) => xs->Belt.Array.reduce(0, (a, b) => a + b)

let product = (xs: array<int>) => xs->Belt.Array.reduce(1, (a, b) => a * b)

exception String_cannot_be_split

let bissect = (c, s) => {
  switch (splitToArray(c, s)) {
  | [l, r] => (l, r)
  | _ => raise(String_cannot_be_split)
  }
}

let bissectMap = (c, s, fn) => {
  switch (splitToArray(c, s)->Belt.Array.map(fn)) {
  | [l, r] => (l, r)
  | _ => raise(String_cannot_be_split)
  }
}

let isBetween = (~value: int, ~min, ~max) => value >= min && value <= max