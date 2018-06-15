%{
int nest_cmt=0;
extern void insert(char *yytext,char type,char *data_type,struct token_list *lexeme);
char *temp;
extern char data_type[20],const_type[20];
extern int nest,scope;
%}

alpha [a-zA-Z_]
digit [0-9]
exp [Ee][+-]?{digit}+
singlecomm (\/\/.*)
comm_beg (\/\*)
comm_end (\*\/)

%x COMM

%%

<*>\n {yylineno = yylineno + 1;}

{singlecomm} {}

{comm_beg} {
BEGIN(COMM);
nest_cmt++;
}

{comm_end} { }

<COMM>{comm_beg} {nest_cmt++;}

<COMM>{comm_end} {
if(nest_cmt>0) nest_cmt--;
if(nest_cmt==0) BEGIN(INITIAL);
}

<COMM>. {}

[ \t] ;


"auto" {return AUTO;}
"break" {return BREAK;}
"case" {return CASE;}
"char" {return CHAR;}
"const" {return CONST;}
"continue" {return CONTINUE;}
"default" {return DEFAULT;}
"do" {return DO;}
"double" {return DOUBLE;}
"else" {return ELSE;}
"enum" {return ENUM;}
"extern" {return EXTERN;}
"float" {return FLOAT;}
"for" {return FOR;}
"goto" {return GOTO;}
"if" {return IF;}
"int" {return INT;}
"long" {return LONG;}
"register" {return REGISTER;}
"return" {return RETURN;}
"short" {return SHORT;}
"signed" {return SIGNED;}
"sizeof" {return SIZEOF;}
"static" {return STATIC;}
"struct" {return STRUCT;}
"switch" {return SWITCH;}
"typedef" {return TYPEDEF;}
"union" {return UNION;}
"unsigned" {return UNSIGNED;}
"void" {return VOID;}
"volatile" {return VOLATILE;}
"while" {return WHILE;}
printf {return PRINTF;}
scanf {return SCANF;}


"#include"(.)* ;
"#define"(.)* ;

{digit}+ {yylval.s=strdup(yytext); strcpy(const_type,"int"); return NUM;}
{digit}*"."{digit}+({exp})? {yylval.s=strdup(yytext); strcpy(const_type,"double"); return NUM;}
{digit}+"."{digit}*({exp})? {yylval.s=strdup(yytext); strcpy(const_type,"double"); return NUM;}
{alpha}({alpha}|{digit})* {yylval.s=strdup(yytext); return ID;}

\"(\\.|[^\\"])*\" {return STRING;}

"++" return INC;
"--" return DEC;
"<=" return LE;
">=" return GE;
"==" return EQ;
"!=" return NE;
">" return GT;
"<" return LT;
"." return DOT;

"{" { nest++;
return yytext[0];
}

"}" {
nest--;
return yytext[0];
}

";" {strcpy(data_type,"-"); return yytext[0];}

. return yytext[0];
%%
