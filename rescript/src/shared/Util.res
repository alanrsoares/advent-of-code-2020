let parseRows = (path) => {
    let file = Node.Fs.readFileAsUtf8Sync(path)

    String.split_on_char('\n', file)->Belt.List.toArray
}

let sum = (xs: array<int>) => xs->Belt.Array.reduce(0, (a, b) => a + b)

let product = (xs: array<int>) => xs->Belt.Array.reduce(1, (a, b) => a * b)


let split = (c, s):(string, string) => {
  let [l, r] = String.split_on_char(c, s)->Belt.List.toArray;
  (l, r)
}

let splitMap = (c, s, fn):('a, 'a) => {
  let [l, r] = String.split_on_char(c, s)->Belt.List.toArray->Belt.Array.map(fn);
  (l, r)
}

let isBetween = (~value: int, ~min, ~max) => value >= min && value <= max