%{
#include <stdio.h>
#include <stdlib.h>
#include<string.h>
int count=0;
extern char *yytext;
extern char *temp;
struct token_list
{
	char *name,token_type[25],dt[20];		
	struct token_list *next;
}*header;
char data_type[20];
void insert(char *yytext,char type);
int yyerror(char*);
int yylex();
%}

%token AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN
%token FLOAT FOR GOTO IF INT LONG REGISTER RETURN SHORT SIGNED SIZEOF STATIC
%token STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE 
%token PRINTF SCANF DOT INC DEC

%token ID NUM STRING

%right '='
%left AND OR
%left '<' '>' LE GE EQ NE LT GT
%left '+' '-'
%left '*' '/' '%'
%right DEC
%left INC

%nonassoc IFX
%nonassoc ELSE

%%

start:	Function 
	| Declaration
	| Function start
	| Declaration start
	;

/* Declaration block */
Declaration: Type Assignment ';' 
	| Assignment ';'  	
	| FunctionCall ';' 	
	| ArrayUsage ';'	
	| Type ArrayUsage ';'   	
	| error
	;

/* Assignment block */
Assignment: identifier '=' Assignment 
	| identifier '=' FunctionCall
	| identifier '=' ArrayUsage
	| ArrayUsage '=' Assignment
	| identifier ',' Assignment
	| identifier ',' ArrayUsage
	| NUM ',' Assignment
	| ID '+' Assignment
	| ID '-' Assignment
	| ID '*' Assignment
	| ID '/' Assignment	
	| number '+' Assignment
	| number '-' Assignment
	| number '*' Assignment
	| number '/' Assignment
	| '\'' Assignment '\''	
	| '(' Assignment ')'
	| '-' '(' Assignment ')'
	| '-' NUM
	| '-' ID
	|   number		
	|identifier
	;

identifier:ID	{insert(temp,'i');}
;

number:NUM	{insert(temp,'c');}
;
/* Function Call Block */
FunctionCall : ID'('')'
	| ID'('Assignment')'
	;

/* Array Usage */
ArrayUsage : identifier'['Assignment']'
|	identifier '[' ']'
	;

/* Function block */
Function: Type ID '(' ArgListOpt ')' CompoundStmt 
	;
ArgListOpt: ArgList
	|
	;
ArgList:  ArgList ',' Arg
	| Arg
	;
Arg:	Type ID
	;
CompoundStmt:	'{' StmtList '}'
	;
StmtList:	StmtList Stmt
	|
	;
Stmt:	WhileStmt
	| Declaration
	| ForStmt
	| IfStmt
	| PrintFunc
	| ScanFunc
	| ReturnStmt
	| CompoundStmt
	| DoWhileStmt
	| ';'	
	;

ReturnStmt: RETURN Expr ';'
;	

/* Type Identifier block */
Type:	INT	{strcpy(data_type,"int");}
	| LONG INT	{strcpy(data_type,"int");}
	| LONG LONG INT	{strcpy(data_type,"int");}
	| FLOAT		{strcpy(data_type,"float");}
	| CHAR		{strcpy(data_type,"char");}
	| DOUBLE	{strcpy(data_type,"double");}
	| LONG DOUBLE	{strcpy(data_type,"double");}
	| VOID 		{strcpy(data_type,"void");}
	;

/* Loop Blocks */ 
WhileStmt: WHILE '(' Expr ')' Stmt
	;

/* For Block */
ForStmt: FOR '(' Expr ';' Expr ';' Expr ')' Stmt
	;

DoWhileStmt: DO Stmt WHILE '(' Expr ')' ';'
;

/* IfStmt Block */
IfStmt : IF '(' Expr ')' Stmt 	%prec IFX
|	IF '(' Expr ')' Stmt ELSE Stmt  
;

/* Print Function */
PrintFunc : PRINTF '(' Expr ')' ';'
	| PRINTF '(' str ')' ';'
	| PRINTF '(' str ',' var ')' ';'
	;

str:STRING	{strcpy(data_type,"string");insert(yytext,'s');}
;

var: ID
|	var ',' ID
;

ScanFunc : SCANF '(' str ',' scanvar ')' ';'
;

scanvar:'&'ID
|	scanvar ',' '&'ID
;

/*Expression Block*/
Expr:	
	| Expr LE Expr 
	| Expr GE Expr
	| Expr NE Expr
	| Expr EQ Expr
	| Expr GT Expr
	| Expr LT Expr
	| INC ID
	| DEC ID	%prec INC
	| ID INC	%prec DEC
	| Assignment
	| ArrayUsage
	;
%%
#include"lex.yy.c"
void insert(char *yytext,char type)
{
	int len1 = strlen(yytext);
	char token_type[25];
	struct token_list *lexeme,*temp,*ptr;
	ptr = header;
	switch(type)
	{
		case 'k':
			strcpy(token_type,"Keyword");
			break;
		case 'i':
			strcpy(token_type,"Identifier");
			break;
		
		case 's':
			strcpy(token_type,"String Literal");
			break;
		case 'c':
			strcpy(token_type,"Constant");
			break;
	}	
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme = ptr;
		if(strcmp(lexeme->name,yytext)==0) break;
	}
	if(i==count)
	{
		temp = (struct token_list*)malloc(sizeof(struct token_list));
		temp->name = (char*)malloc((len1+1)*sizeof(char));
		strcpy(temp->name,yytext);
		strcpy(temp->token_type,token_type);
		strcpy(temp->dt,data_type);
		temp->next = NULL;
		if(count==0) header=temp;
		else lexeme->next = temp;
		count++;
		fprintf(yyout,"\n%35s %30s %20s",temp->name,temp->token_type,temp->dt);
		strcpy(data_type,"-----");
	}
}

int main(int argc, char *argv[])
{
	if(argc==1) printf("Arguments missing ! correct format : ./a.out filename \n");
	else
	{
		yyin = fopen(argv[1], "r");
		yyout = fopen("output.txt","w");
		fprintf(yyout,"\t\t\t--------------------------------------------\n");
		fprintf(yyout,"\t\t\t\t\tSYMBOL TABLE\n");
		fprintf(yyout,"\t\t\t--------------------------------------------\n");
		fprintf(yyout,"\t\t\tLexeme\t\t\t\tToken\t\t\t\tData Type\n");
		if(!yyparse())
			printf("\nParsing complete\n");
		else
			printf("\nParsing failed\n");
		fclose(yyout);
		fclose(yyin);
	}
	return 0;
}
         
int yyerror(char *s) {
	printf("Line %d : %s %s\n", yylineno, s, yytext );
}
