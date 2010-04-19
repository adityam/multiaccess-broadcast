import Text.Pandoc

extract :: Block -> Block
extract buffer@(CodeBlock (id, classes, namevals) contents) = buffer
extract _ = Para [Space]

readDoc :: String -> Pandoc
readDoc = readMarkdown defaultParserState

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown defaultWriterOptions 

main :: IO ()
main = interact (writeDoc . processWith extract . readDoc) 
  

