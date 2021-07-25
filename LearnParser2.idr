import Text.Parser
import Text.Token


%default total

-- foo : Int
-- foo = foo

emptyGrammar : Grammar Int False Bool
emptyGrammar = Empty True

testEmptyGrammar : Either (ParseError Int) (Bool, List Int)
testEmptyGrammar = parse emptyGrammar [1,2,3,4,5]

match : Char -> Char -> Maybe ()
match x y = if x == y then Just () else Nothing


-- aaaaaaaaaaaaaaaaaaaaaaaa..... c
infGrammar : Grammar Char True ()
infGrammar = 
  do 
    terminal "the_a" (match 'a')
    (terminal "the_c" (match 'c') <|> infGrammar)
    -- (terminal "the_c" (match 'c') `Text.Parser.Core.(<|>)` infGrammar) -- note, this also works
        

-- aaaaaaaaaaaaaaaaaaaaaaaa..... (with no end, not useful, I guess)
infGrammar2 : Grammar Char True ()
infGrammar2 = 
  do 
    terminal "the_a" (match 'a')
    infGrammar2
        
fooGrammar : Grammar Char True ()
fooGrammar = 
  (>>=) {c2=True} (terminal "the_a" (match 'a'))
    (\a => do 
             if a /= () then  -- this does nothing, just shows that if's can be inserted
                              -- if necessary
                 (do
                    terminal "the_b" (match 'b')
                    pure ())
               else
                 (do
                    terminal "the_b" (match 'b')
                    commit --commit here forces the branch
                    terminal "the_c" (match 'c')) 
                  <|>
                    (do
                      terminal "the_x" (match 'x')
                      terminal "the_y" (match 'y')
                      ))
         
--Left (Error "the_c" ['z'])
-- testE : String          
-- testE = show $ parse fooGrammar (unpack "abz")

isRightAndEq : Eq y => Either x y -> y -> Bool
isRightAndEq (Left _) _ = False
isRightAndEq (Right y) y2 = y == y2

test : List Bool
test = [parse fooGrammar (unpack "abc") `isRightAndEq` ((),[]) 
       ,parse fooGrammar (unpack "axy") `isRightAndEq` ((),[]) 
       ]
