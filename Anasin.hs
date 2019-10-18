module Anasin
( anasin
) where

exp' :: [([Char], [Char])] -> [([Char], [Char])]
exp' [] = []
exp' (y:ys)
    | ((fst y) == "PARENTESES_ESQ") = [("Entrou em exp(), ", "")] ++ [("PARENTESES_ESQ", "(")] ++ expression ys
    | (((fst y) == "INTEIRO" || (fst y) == "IDENTIFICADOR") && (not(null ys)) && (fst(head ys)) == "PARENTESES_DIR") = [("Entrou em exp(), ", "")] ++ [y] ++ [(head ys)]
    | ((fst y) == "IDENTIFICADOR" && null ys) = [("Entrou em exp(), ", "")] ++ [y]
    | ((fst y) == "INTEIRO" && null ys) = [("Entrou em exp(), ", "")] ++ [y]
    | ((fst y) == "PONTO FLUTUANTE" && null ys) = [("Entrou em exp(), ", "")] ++ [y]
    | otherwise = [("ERROR", "lexema final nao identificado!")]

factor :: [([Char], [Char])] -> [([Char], [Char])] 
factor [] = [("ERROR em stmtlist", "Nao existe expressao apos a operacao de **")]
factor (ys) = if (null(dropWhile (/=("OPERADOR_EXPONENCIACAO", "**")) ys))
    then [("Entrou em factor(), ", "")] ++ exp' (ys) ++ [("Saiu de factor(), ", "")]
    else [("Entrou em factor(), ", "")] ++ exp' (takeWhile (/=("OPERADOR_EXPONENCIACAO", "**")) ys) ++ [head(dropWhile (/=("OPERADOR_EXPONENCIACAO", "**")) ys)] ++ factor (tail(dropWhile (/=("OPERADOR_EXPONENCIACAO", "**")) ys))

auxterm :: [([Char], [Char])] -> Int
auxterm [] = 0
auxterm (ys)
    | (length(dropWhile (/=("OPERADOR_MULTIPLICACAO", "*")) ys)) > (length(dropWhile (/=("OPERADOR_DIVISAO", "/")) ys)) = 1
    | (length(dropWhile (/=("OPERADOR_MULTIPLICACAO", "*")) ys)) < (length(dropWhile (/=("OPERADOR_DIVISAO", "/")) ys)) = 2
    | otherwise = 0

term :: [([Char], [Char])] -> [([Char], [Char])]
term [] = [("ERROR em stmtlist", "Nao existe expressao apos a operacao de * ou /")]
term (ys) 
    | (auxterm ys == 1) = [("Entrou em term(), ", "")] ++ factor (takeWhile (/=("OPERADOR_MULTIPLICACAO","*")) ys) ++ [head(dropWhile (/=("OPERADOR_MULTIPLICACAO", "*")) ys)] ++ term (tail(dropWhile (/=("OPERADOR_MULTIPLICACAO","*")) ys))
    | (auxterm ys == 2) = [("Entrou em term(), ", "")] ++ factor (takeWhile (("OPERADOR_DIVISAO","/")<) ys) ++ [head(dropWhile (("OPERADOR_DIVISAO", "/")<) ys)] ++ term (tail(dropWhile (("OPERADOR_DIVISAO","/")<) ys))
    | (auxterm ys == 0) = [("Entrou em term(), ", "")] ++ factor (ys) ++ [("Saiu de term(), ", "")]

auxexpression :: [([Char], [Char])] -> Int
auxexpression [] = 0
auxexpression (ys)
    | (length(dropWhile (/=("OPERADOR_SOMA", "+")) ys)) > (length(dropWhile (/=("OPERADOR_SUBTRACAO", "-")) ys)) = 1
    | (length(dropWhile (/=("OPERADOR_SOMA", "+")) ys)) < (length(dropWhile (/=("OPERADOR_SUBTRACAO", "-")) ys)) = 2
    | otherwise = 0

expression :: [([Char], [Char])] -> [([Char], [Char])]
expression [] = [("ERROR em stmtlist", "Nao existe expressao apos a operacao de + ou -")]
expression (ys)
    | (auxexpression ys == 1) = [("Entrou em expression(), ", "")] ++ term (takeWhile (/=("OPERADOR_SOMA","+")) ys) ++ [head(dropWhile (/=("OPERADOR_SOMA", "+")) ys)] ++ expression (tail(dropWhile (/=("OPERADOR_SOMA","+")) ys))
    | (auxexpression ys == 2) = [("Entrou em expression(), ", "")] ++ term (takeWhile (("OPERADOR_SUBTRACAO","-")<) ys) ++ [head(dropWhile (("OPERADOR_SUBTRACAO", "-")<) ys)] ++ expression (tail(dropWhile (("OPERADOR_SUBTRACAO","-")<) ys))
    | (auxexpression ys == 0) = [("Entrou em expression(), ", "")] ++ term (ys) ++ [("Saiu de expression(), ", "")]
 
stmtaux [] = [("ERROR em stmtlist", "Nao existe expressao apos a atribuicao")]
stmtaux ys = expression ys

stmt :: [([Char], [Char])] -> [([Char], [Char])]     
stmt [] = []
stmt (x:y:ys) = if fst x == "IDENTIFICADOR" && fst y == "OPERADOR_ATRIBUICAO"
    then [("Entrou em stmt(), ", (fst x ++ ": " ++ snd x ++ " "))] ++ [("Entrou em stmt(), ", (fst y ++ ": " ++ snd y ++ " "))] ++ stmtaux (ys) ++ [("Saiu de stmt(), ", "")]
    else [("ERROR em stmt()", "Atribuicao de valores feita de forma incorreta")]

stmtlist :: [([Char], [Char])] -> [([Char], [Char])] 
stmtlist [] = [("ERROR em stmtlist", "Nao existe expressao apos o ;")]
stmtlist (ys) = if (null(dropWhile (/=("PONTO_VIRGULA", ";")) ys))
    then [("Entrou em stmtlist(), ", "")] ++ stmt (ys) ++ [("Saiu de stmtlist(), ", "")]
    else [("Entrou em stmtlist(), ", "")] ++ (stmt (takeWhile (/=("PONTO_VIRGULA", ";")) ys)) ++ [head(dropWhile (/=("PONTO_VIRGULA", ";")) ys)] ++ stmtlist (tail(dropWhile (/=("PONTO_VIRGULA", ";")) ys))
 
program :: [([Char], [Char])] -> [([Char], [Char])] 
program [] = []
program (y:ys) = if (((fst y) == "MARCADOR") && ((snd y) == "begin")) && ((fst(last ys) == "MARCADOR") && (snd(last ys) == "end"))
    then [("Entrou em program(), ", (fst y ++ ": " ++ snd y))] ++ stmtlist (take ((length ys)-1) ys) ++ [("Saiu de program()", (fst(last ys) ++ ": " ++ snd(last ys)))]
    else [("ERROR em program()", "O escopo do codigo nao foi definido corretamente!")]

anasin :: [([Char], [Char])] -> [([Char], [Char])]
anasin (ys) = if (not(null ys))
    then program (ys)
    else [("ERROR", "Lista de tokens e lexemas esta vazia!")]
