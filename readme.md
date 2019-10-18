### Analisador Léxico(Scanner) e um Analisador Sintático(Parser) 


LINGUAGENS DE PROGRAMAÇÃO – TRABALHO 01

Seja a seguinte gramática livre de contexto:

```
<program> → begin <stmt_list> end

<stmt_list> → <stmt> 
             | <stmt> ; <stmt_list>

<stmt> → ID = <expression>

<expression> → <expression> + <term> 
              | <expression> – <term> 
              | <term>

<term> → <term> * <factor> 
        | <term> / <factor> 
        | <factor>

<factor> → <exp> ** <factor> 
         | <exp>

<exp> → ID 
       | constante_inteira 
       | constante_inteira . constante_inteira 
       | (<expression>)
```

Onde:
* ID representa um identificador (lexema) que poder iniciar com qualquer letra seguido de qualquer letra ou número.
* constante_inteira representa um número inteiro.
* constante_inteira . constante_inteira representa um número em ponto flutuante, onde a parte decimal é separada por “.”(ponto).

Implemente e execute, em qualquer linguagem de programação:

* Um analisador léxico;
* Um analisador sintático descendente recursivo;
* Pelo menos três exemplos de programas compatíveis com a gramática acima.

