Lambda Calculus Interpreter
    The program implement a lambda calculus interpreter in a functional
    programming language to reduce lambda calculus expressions in a call-by-name
    (normal order) manner.

Team member: Haotian Wu, Minghua Zhang
Feature: - Normal order lambda calculus inteperter
         - Alpha renaming(by appending a "1" to the variable which needs to rename),
         - Beta reduction
         - Eta reduction
Bug: - If the program read in an expression which its type is not define within the code
       then it will result in an error message
     - Can not do applicative order reduction
     - Code can be improved to a better structure