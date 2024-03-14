CSE 341 HW2

PART I:

If you want to test .l file  

run:

    flex gpp_lexer.l
    gcc lex.yy.c
    ./a.out /* if you want to make it read from stdin */
    ./a.out filename /* if you want to read from file */

PART II:

If you want to test .lisp file

run:
    
    clisp gpp_lexer.lisp /* install clisp if you don't have. if you want to make it read from terminal */
    clisp gpp_lexer.lisp filename /* if you want to read from file */


