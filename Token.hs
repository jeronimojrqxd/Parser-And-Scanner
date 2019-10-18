module Token
( idtoken
) where 

idtoken :: Char -> String -> String
idtoken x a
        | x == parenteses_esq = "PARENTESES_ESQ"
        | x == parenteses_dir = "PARENTESES_DIR"
        | x == operador_soma = "OPERADOR_SOMA"
        | x == operador_sub = "OPERADOR_SUBTRACAO"
        | x == operador_ast = exceptionast a 
        | x == operador_div = "OPERADOR_DIVISAO"
        | x == operador_atr = "OPERADOR_ATRIBUICAO"
        | x == ponto_virgula = "PONTO_VIRGULA"
        | elem x identificador = "IDENTIFICADOR"
        | elem x literal = exceptionliteral a
        | otherwise = "ERROR"
        where r = x
              parenteses_esq = '('
              parenteses_dir = ')'
              operador_soma = '+'
              operador_sub = '-'
              operador_ast = '*'
              operador_div = '/'
              operador_atr = '='
              ponto_virgula = ';'
              identificador = ['a'..'z']++['A'..'Z']
              literal = ['0'..'9']

exceptionast :: String -> String
exceptionast [] = "OPERADOR_MULTIPLICACAO"
exceptionast (a:as) = if a == '*'
                    then "OPERADOR_EXPONENCIACAO"
                    else "OPERADOR_MULTIPLICACAO"

exceptionliteral :: String -> String
exceptionliteral [] = "INTEIRO"
exceptionliteral (a:as)
    | a == '.' = "PONTO_FLUTUANTE"
    | elem a tst = exceptionliteral as
    | otherwise = "INTEIRO"
    where tst = ['0'..'9']