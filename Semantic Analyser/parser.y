%{
#include <stdio.h>
#include <stdlib.h>
#include<string.h>
int count=0,mainscope=9999,flag=0,total=0,call=0,state=0;
extern char *yytext;
extern char *temp;
struct token_list
{
	char *name,token_type[25],dt[20],dimension[20];		
	struct token_list *next;
	int nest,scope,parameter;
}*header,*footer;
char data_type[20],const_type[20],func_type[20],fname[20];
void lookup(char *yytext,char type,char *data_type);
void insert(char *yytext,char type,char *data_type,struct token_list *lexeme);
void check(char *yytext);
int nest=0,scope=0;
int yyerror(char*);
void returntypecheck(char *yytext);
int yylex();
void checkforvoid();
void copy();
void parametercheck(char *s);
void arrdim(char *arr,char *dim);
void typeforvoid();
void typecheck(char *v1, char * v2);

%}

%token AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN
%token FLOAT FOR GOTO IF INT LONG REGISTER RETURN SHORT SIGNED SIZEOF STATIC
%token STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE 
%token PRINTF SCANF DOT INC DEC

%union
{
	char *s;
}

%token <s>ID <s>NUM <s>STRING

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
	| start Function
	| start Declaration 
	;

/* Declaration block */
Declaration: Type Assignment ';' 	
	| Type ArrayUsage ';' 
	| error 
	;

/* Assignment block */
Assignment: identifier '=' arith {typecheck($<s>1,$<s>3);} ',' Assignment
	| identifier '=' arith	{typecheck($<s>1,$<s>3);}
	| identifier '=' FunctionCall
	| identifier '=' ArrayUsage
	| ArrayUsage '=' arith
	| identifier ',' Assignment
	| identifier ',' ArrayUsage	
	| '(' Assignment ')'
	| '-' '(' Assignment ')'
	| '-' number
	| number		
	| identifier
	;

identifier:ID	{lookup($1,'i',data_type);}
;

number:NUM	{/*lookup($1,'c',const_type);*/}
;

arith: ID '+' arith	{check($1);}
	| ID '-' arith	{check($1);}
	| ID '*' arith	{check($1);}
	| ID '/' arith	{check($1);}
	| '-' ID	{check($2);}
	| NUM '+' arith	{check($1);}
	| NUM '-' arith	{check($1);}
	| NUM '*' arith	{check($1);}
	| NUM '/' arith	{check($1);}
	| ID 		{check($1); $<s>$=$1;}
	| NUM
;

/* Function Call Block */
FunctionCall : ID'('')'		{check($1);parametercheck($1); call=0;}
	| ID'('argslis')'	{check($1);parametercheck($1); call=0;}
	;

argslis:argslis ',' args
|	args
;

args: number		{call++;}
|	identifier	{call++;}
;
/* Array Usage */
ArrayUsage : identifier'['Assignment']' {if(atoi($<s>3)<=0 || atof($<s>3)>atoi($<s>3)) yyerror("Semantic Error Undefined Array dimension");
						else arrdim($<s>1,$<s>3);
					}
	//|	identifier '[' ']'
	;
	
/* Function block */
Function: Type ID {strcpy(func_type,data_type); lookup($2,'f',data_type);} {nest++;} '(' ArgListOpt ')' {nest--; if(state==0) copy();
												else state=0;total=0;} CompoundStmt 
	;
ArgListOpt: ArgList
	|
	;
ArgList:  ArgList ',' Arg
	| Arg
	;
Arg:	Type identifier {typeforvoid(); total++;}
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
	| FunctionCall ';'
	| Expr ';'
	| ';'
	;

ReturnStmt: RETURN Expr ';' {returntypecheck($<s>2);}
	| RETURN ';'	{checkforvoid();}
;	

/* Type Identifier block */
Type:	INT		{strcpy(data_type,"int");}
	| LONG INT	{strcpy(data_type,"int");}
	| LONG LONG INT	{strcpy(data_type,"int");}
	| FLOAT		{strcpy(data_type,"float");}
	| CHAR		{strcpy(data_type,"char");}
	| DOUBLE	{strcpy(data_type,"double");}
	| LONG DOUBLE	{strcpy(data_type,"double");}
	| VOID 		{strcpy(data_type,"void");}
	;

FE:	
|	Expr
;

/* Loop Blocks */ 
WhileStmt: WHILE '(' Expr ')' Stmt
	;

/* For Block */
ForStmt: FOR '(' FE ';' FE ';' FE ')' Stmt
	;

DoWhileStmt: DO Stmt WHILE '(' Expr ')' ';'
;

/* IfStmt Block */
IfStmt : IF '(' Expr ')' Stmt 	%prec IFX
|	IF '(' Expr ')' Stmt ELSE Stmt  
;

/* Print Function */
PrintFunc : PRINTF '(' str ')' ';'
	| PRINTF '(' str ',' var ')' ';'
	;

str:STRING	{/*strcpy(const_type,"string");lookup(yytext,'s',const_type);*/}
;

var: identifier
| ArrayUsage
|	var ',' identifier
| var ',' ArrayUsage
;

ScanFunc : SCANF '(' str ',' scanvar ')' ';'
;

scanvar:'&'identifier
|	scanvar ',' '&'identifier
|	'&'identifier'['Assignment']'
|	scanvar ',' '&'identifier'['Assignment']'
;

/*Expression Block*/
Expr:	Expr LE Expr 
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
void typecheck(char *v1, char * v2)
{
	struct token_list *lexeme1,*lexeme2,*ptr;
	ptr =lexeme1=lexeme2= header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		if(!(strcmp(lexeme1->name,v1)==0 && lexeme1->nest<=nest && (lexeme1->scope==0 || lexeme1->scope==scope))) lexeme1=ptr;
		if(!(strcmp(lexeme2->name,v2)==0 && lexeme2->nest<=nest && (lexeme2->scope==0 || lexeme2->scope==scope))) lexeme2=ptr;
		if((strcmp(lexeme1->name,v1)==0 && lexeme1->nest<=nest && (lexeme1->scope==0 || lexeme1->scope==scope)) && (strcmp(lexeme2->name,v2)==0 && lexeme2->nest<=nest && (lexeme2->scope==0 || lexeme2->scope==scope)))
		{
			if(strcmp(lexeme1->dt,lexeme2->dt)!=0) printf("Line %d : Semantic Error Type Mismatch %s=%s\n",yylineno,v1,v2);
			break;
		}
	}
}
void typeforvoid()
{
	if(strcmp(data_type,"void")==0) printf("Line %d : Semantic Error Data Type cannot be void\n",yylineno);
}
void arrdim(char *arr,char *dim)
{
	struct token_list *lexeme,*ptr;
	ptr = header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme=ptr;
		if(strcmp(lexeme->name,arr)==0 && lexeme->scope==scope && lexeme->nest==nest)
		{
			strcpy(lexeme->token_type,"Array");
			strcpy(lexeme->dimension,dim);
			break;
		}
	}
}
void parametercheck(char *s)
{
	struct token_list *lexeme,*ptr;
	ptr = header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme=ptr;
		if(strcmp(lexeme->name,s)==0)
		{
			if(lexeme->parameter!=call) printf("Line %d : Semantic Error Parameter Mismatch\n",yylineno);
			break;
		}
	}
}
void checkforvoid()
{
	if(strcmp(func_type,"void")!=0) printf("Line %d : Semantic Error Type Mismatch\n",yylineno);
}
void returntypecheck(char *yytext)
{
	struct token_list *lexeme,*ptr;
	ptr = header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme=ptr;
		if(strcmp(lexeme->name,yytext)==0 && lexeme->nest<=nest && (lexeme->scope==0 || lexeme->scope==scope) && strcmp(lexeme->dt,func_type)==0) break;
	}
	if(i==count) printf("Line %d : Semantic Error Type Mismatch %s\n",yylineno,yytext);
}
void check(char *yytext)
{
	struct token_list *lexeme,*ptr;
	ptr = header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme=ptr;
		if(strcmp(lexeme->name,yytext)==0 && lexeme->nest<=nest && (lexeme->scope==0 || lexeme->scope==scope)) break;	
	}
	if(i==count) printf("Line %d : Semantic Error Undeclared variable %s\n",yylineno,yytext);
}

void lookup(char *yytext,char type,char *data_type)
{
	struct token_list *lexeme,*ptr;
	ptr = header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme = ptr;
		if(strcmp(lexeme->name,yytext)==0 && (lexeme->scope==0 || lexeme->scope==scope))
		{
			if(strcmp(data_type,"-")!=0 && lexeme->scope==scope && lexeme->nest==nest)
			{
				printf("Line %d : Semantic Error Redeclaration of variable %s\n",yylineno,yytext);
				if(type=='f') state=1;
				break;
			}
			else if(strcmp(data_type,"-")==0 && (lexeme->scope==0 || lexeme->scope==scope) && lexeme->nest<=nest) break;
		}
	}
	if(i==count)
	{
		if(strcmp(data_type,"-")==0) printf("Line %d : Semantic Error Undeclared Variable %s\n",yylineno,yytext);
		else insert(yytext,type,data_type,footer);
	}
}
void insert(char *yytext,char type,char *data_type,struct token_list *lexeme)
{
	int len1 = strlen(yytext);
	char token_type[25];
	struct token_list *temp;
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
		case 'f':
			strcpy(token_type,"Function");
			scope++;
			strcpy(fname,yytext);
			if(strcmp(yytext,"main")==0) mainscope=scope;
			if(scope>mainscope)
			{
				printf("Line %d : Semantic Error Function main should be last function %s\n",yylineno,yytext);
				exit(0);
			}
			break;
	}	
	temp = (struct token_list*)malloc(sizeof(struct token_list));
	temp->name = (char*)malloc((len1+1)*sizeof(char));
	strcpy(temp->name,yytext);
	strcpy(temp->token_type,token_type);
	strcpy(temp->dt,data_type);
	strcpy(temp->dimension,"-");
	temp->nest=nest;
	temp->scope=scope;
	temp->parameter=0;
	temp->next = NULL;
	if(count==0) header=temp;
	else lexeme->next = temp;
	count++;
	//fprintf(yyout,"\n%35s %30s %30s %10d %10d",temp->name,temp->token_type,temp->dt,temp->scope,temp->nest);
	footer=temp;
}
void copy()
{
	struct token_list *lexeme,*ptr;
	ptr = header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme=ptr;
		if(strcmp(lexeme->name,fname)==0)
		{
			lexeme->parameter=total;
			break;
		}
	}
}
int main(int argc, char *argv[])
{
	if(argc==1) printf("Arguments missing ! correct format : ./a.out filename \n");
	else
	{
		yyin = fopen(argv[1], "r");
		yyout = fopen("output.txt","w");
		fprintf(yyout,"\t\t\t\t\t\t--------------------------------------------\n");
		fprintf(yyout,"\t\t\t\t\t\t\t\tSYMBOL TABLE\n");
		fprintf(yyout,"\t\t\t\t\t\t--------------------------------------------\n");
		fprintf(yyout,"\t\tLexeme\t\tToken\t\t\tData Type\tScope\t  nest\t   Parameter\tDimension\n");
		int x=yyparse();
		struct token_list *lexeme,*ptr;
		ptr = header;
		int i;
		for(i=0;i<count;i++,ptr=ptr->next)
		{
			lexeme=ptr;
			fprintf(yyout,"\n%20s %20s %20s %13d %8d %10d %10s",lexeme->name,lexeme->token_type,lexeme->dt,lexeme->scope,lexeme->nest,lexeme->parameter,lexeme->dimension);
		}
		if(!x)
		{
			
			printf("\nParsing complete\n");
		}
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
