import Text.Pandoc

extract :: Block -> [String]
extract (CodeBlock (id, classes, namevals) contents) = [contents]
extract _ = []

readDoc :: String -> Pandoc
readDoc = readMarkdown defaultParserState

main :: IO ()
main = interact (unlines .queryWith extract . readDoc)
  

