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
    (terminal "the_c" (match 'c') <|>
       infGrammar)
        

-- aaaaaaaaaaaaaaaaaaaaaaaa..... (with no end, not useful, I guess)
infGrammar2 : Grammar Char True ()
infGrammar2 = 
  do 
    terminal "the_a" (match 'a')
    infGrammar2
        
