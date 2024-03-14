%{
#include <stdio.h>
#include <stdlib.h>
%}


digit [0-9]
letter [a-zA-Z]
id [a-zA-Z][a-zA-Z0-9]* 
error_token {digit}+{letter}+


%%
{error_token} printf("SYNTAX_ERROR %s can not be tokenized.\n", yytext); /* error token is a combination of digit and letter*/
{digit}+"b"{digit}+ {printf("VALUEF\n");} /* only predefined literal is valuef and it explains unsigned fractions*/

and {printf("KW_AND\n");} /* keywords are defined here*/
or {printf("KW_OR\n");}
not {printf("KW_NOT\n");}
equal {printf("KW_EQUAL\n");}
less {printf("KW_LESS\n");}
nil {printf("KW_NIL\n");}
list {printf("KW_LIST\n");}
append {printf("KW_APPEND\n");}
concat {printf("KW_CONCAT\n");}
set {printf("KW_SET\n");}
def {printf("KW_DEF\n");}
for {printf("KW_FOR\n");}
if {printf("KW_IF\n");}
exit {printf("KW_EXIT\n");}
load {printf("KW_LOAD\n");}
display {printf("KW_DISPLAY\n");}
true {printf("KW_TRUE\n");}
false {printf("KW_FALSE\n");}


{id} {printf("IDENTIFIER\n");} /* identifiers are defined here*/

"+" {printf("OP_PLUS\n");}  /* operators are defined here*/
"-" {printf("OP_MINUS\n");}
"/" {printf("OP_DIV\n");}
"*" {printf("OP_MULT\n");}
"(" {printf("OP_OP\n");}
")" {printf("OP_CP\n");}
"," {printf("OP_COMMA\n");}

";;".* {printf("COMMENT\n");} /* comments are defined here*/


[ \t\n]+   /* ignoring white space*/

. printf("SYNTAX_ERROR %s can not be tokenized.\n", yytext); /* if the input is not defined above, it is an error*/


%%
int yywrap() 
{
    return 1;
}



int main(int argc, char** argv){
    if(argc  >= 2 )
    {
        yyin = fopen(argv[1], "r");
    }
        else if (argc == 1)
        {
            yyin = stdin;
            printf(">");
        }
    yylex();

    

return 0;
}

 