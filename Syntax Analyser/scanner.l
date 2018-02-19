%{
	int nest_cmt=0;
	extern void insert(char *yytext,char type);
	void copy(char *yytext);
	char *temp;
	extern char data_type[20];
%}

alpha [a-zA-Z_]
digit [0-9]
exp	[Ee][+-]?{digit}+
singlecomm 	(\/\/.*)
comm_beg 	(\/\*)
comm_end 	(\*\/)

%x COMM

%%

<*>\n   {yylineno = yylineno + 1;}

{singlecomm}	{}

{comm_beg}	{  	
			BEGIN(COMM);
			nest_cmt++;
	        }

{comm_end}	{ }

<COMM>{comm_beg}	{nest_cmt++;}

<COMM>{comm_end} 	{
		                if(nest_cmt>0)	nest_cmt--;
		                if(nest_cmt==0)	BEGIN(INITIAL);
               		}

<COMM>. {}

[ \t]		;


"auto"		{return AUTO;}
"break"		{return BREAK;}
"case"		{return CASE;}
"char"		{return CHAR;}
"const"		{return CONST;}
"continue"	{return CONTINUE;}
"default"	{return DEFAULT;}
"do"		{return DO;}
"double"	{return DOUBLE;}
"else"		{return ELSE;}
"enum"		{return ENUM;}
"extern"	{return EXTERN;}
"float"		{return FLOAT;}
"for"		{return FOR;}
"goto"		{return GOTO;}
"if"		{return IF;}
"int"		{return INT;}
"long"		{return LONG;}
"register"	{return REGISTER;}
"return"	{return RETURN;}
"short"		{return SHORT;}
"signed"	{return SIGNED;}
"sizeof"	{return SIZEOF;}
"static"	{return STATIC;}
"struct"	{return STRUCT;}
"switch"	{return SWITCH;}
"typedef"	{return TYPEDEF;}
"union"		{return UNION;}
"unsigned"	{return UNSIGNED;}
"void"		{return VOID;}
"volatile"	{return VOLATILE;}
"while"		{return WHILE;}
printf  return PRINTF;
scanf return SCANF;


"#include"(.)*	;
"#define"(.)*	;

{digit}+       {{strcpy(data_type,"int");}copy(yytext);return NUM;}
{digit}*"."{digit}+({exp})?	{{strcpy(data_type,"double");}copy(yytext);return NUM;}
{digit}+"."{digit}*({exp})?	{{strcpy(data_type,"double");}copy(yytext);return NUM;}
{alpha}({alpha}|{digit})* {copy(yytext);return ID;}	

\"(\\.|[^\\"])*\"	{return STRING;}


"++"	return INC;
"--"	return DEC;
"<="    return LE;
">="    return GE;
"=="    return EQ;
"!="    return NE;
">"	return GT;
"<"	return LT;
"."     return DOT;

.       return yytext[0];
%%

void copy(char *yytext)
{
	int len = strlen(yytext);
	temp = (char*)malloc((len+1)*sizeof(char));
	strcpy(temp, yytext);
}
