require('gen').prompts['Docs Generate'] = {
  prompt = "You are an expert software engineer and rust programmer. Generate docs, but not code, where necessary in the folloinng rust code. Be concise and do not miss the given struct. You don't need to generate or assume other structs in the fields.\n$text",
  replace = true,
  extract = "```$filetype\n(.-)```",
}
