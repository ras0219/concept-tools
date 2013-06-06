import Concepts
import Data.Set
---------------------------------
-- The concept we are solving for
c1 = fromList [ ForwardIterator (Type "I") ]
---------------------------------

c2 = simplify c1

main = do
  putStrLn "What are the concepts for ForwardIterator{I}?"
  putStrLn $ showConcepts $ c2
  putStrLn ""
  putStrLn "Do these satisfy InputIterator{I}?"
  putStrLn $ show $ satisfies c2 $ fromList [InputIterator (Type "I")]
