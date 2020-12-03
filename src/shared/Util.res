let parseRows = (path) => {
    let file = Node.Fs.readFileAsUtf8Sync(path);
      
    String.split_on_char('\n', file)->Belt.List.toArray
}