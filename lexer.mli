exception LexerError of bytes
val create: unit -> Lexing.lexbuf -> Parser.token
