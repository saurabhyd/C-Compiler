%{
#include <stdio.h>
#include <stdlib.h>
#include<string.h>
int count=0,mainscope=9999,flag=0,total=0,call=0,state=0,tmpvar=0,lblvar=0,base=100;
char *midresult,*midlabel;
extern char *yytext;
extern char *temp;
FILE *icg;
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
char* arrdim(char *arr,char *dim);
void arrdimension(char *arr,char *dim);
void typeforvoid();
void typecheck(char *v1, char * v2);
char* gencode(char *p1,char *op,char *p2);
char* genfor(char *lhs,char *p1,char *op,char *p2);

struct node* genlabel();
struct node* genlabelfor();
struct node
{
	char *str;
	int addr;
	long int pos;
	struct node *next;
}*head;

void backpatch(long int labelpos,struct node* n);

%}

%token AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN
%token FLOAT FOR GOTO IF INT LONG REGISTER RETURN SHORT SIGNED SIZEOF STATIC
%token STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE 
%token PRINTF SCANF DOT INC DEC

%union
{
	char *s;
	struct node *n;
}

%token <s>ID <s>NUM <s>STRING

%right '='
%left '<' '>' LE GE EQ NE LT GT
%left '+' '-'
%left '*' '/' '%'
%left '(' ')'
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
	| Type array ';' 
	| error 
	;

/* Assignment block */
Assignment: identifier '=' arith {typecheck($<s>1,$<s>3); fprintf(icg,"%d: %s = %s;\n",base++,$<s>1,$<s>3);} ',' Assignment
	| identifier '=' arith	{typecheck($<s>1,$<s>3); fprintf(icg,"%d: %s = %s;\n",base++,$<s>1,$<s>3);}
	| identifier '=' FunctionCall
	| identifier '=' ArrayUsage	{fprintf(icg,"%d: %s = *%s\n",base++,$<s>1,$<s>3);}
	| ArrayUsage '=' arith		{fprintf(icg,"%d: *%s = %s\n",base++,$<s>1,$<s>3);}
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

arith: arith '+' arith	{$<s>$=gencode($<s>1,"+",$<s>3);}
	| arith '-' arith {$<s>$=gencode($<s>1,"-",$<s>3);}
	| arith '*' arith {$<s>$=gencode($<s>1,"*",$<s>3);}
	| arith '/' arith {$<s>$=gencode($<s>1,"/",$<s>3);}
	| '(' arith ')' {$<s>$=$<s>2;}
	| '-' ID	{check($2); $<s>$=gencode("-1","*",$<s>2);}
	| ID 		{check($1); $<s>$=$1;}
	| NUM		{$<s>$=$1;}
;

/* Function Call Block */
FunctionCall : ID'('')'		{check($1); parametercheck($1); call=0; fprintf(icg,"%d: LCall %s\n%d: PopParameters\n",base++,$1,base++);}
	| ID'('argslis')'	{check($1); parametercheck($1); call=0; fprintf(icg,"%d: LCall %s\n%d: PopParameters\n",base++,$1,base++);}
	;

argslis:argslis ',' args
|	args
;

args: number		{call++; fprintf(icg,"%d: PushParameter %s\n",base++,$<s>1);}
|	identifier	{call++; fprintf(icg,"%d: PushParameter %s\n",base++,$<s>1);}
;
/* Array Usage */
array: identifier'['Assignment']' {if(atoi($<s>3)<=0 || atof($<s>3)>atoi($<s>3)) yyerror("Semantic Error Undefined Array dimension");
						else arrdimension($<s>1,$<s>3);
					}
;

ArrayUsage : identifier'['Assignment']' {if((atoi($<s>3)<=0 || atof($<s>3)>atoi($<s>3)) && $<s>$[0]<65) yyerror("Semantic Error Undefined Array dimension");
						else $<s>$=arrdim($<s>1,$<s>3);
					}
	//|	identifier '[' ']'
	;
	
/* Function block */
Function: Type ID {strcpy(func_type,data_type); lookup($2,'f',data_type); fprintf(icg,"\n%d: %s:",base++,$2); fprintf(icg,"\n\n%d: BeginFunc;\n",base++); nest++;} '(' ArgListOpt ')'
												{nest--; if(state==0) copy();
								else state=0;total=0;} CompoundStmt {fprintf(icg,"%d: EndFunc;\n",base++);}
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

/* Loop Blocks */ 
WhileStmt: whilelabel {fprintf(icg,"\n%d: %s:\n",base++,$<n>1->str);} 
	'(' 
	Expr {fprintf(icg,"%d: ifZ %s Goto: %s: ",base++,$<s>4,$<n>1->next->str); $<n>1->next->pos=ftell(icg);  fprintf(icg,"___\n");}
	')'  
	Stmt {	fprintf(icg,"%d: Goto: %s: %d",base++,$<n>1->str,$<n>1->addr);
		fprintf(icg,"\n%d: %s:\n",base++,$<n>1->next->str); 
		$<n>1->next->addr=base-1;
		backpatch($<n>1->next->pos,$<n>1->next); }
	;

whilelabel: WHILE {$<n>$=genlabel();}
;

/* For Block */
ForStmt: forlabel '(' FE {fprintf(icg,"\n%d: %s:\n",base++,$<n>1->str);} ';' FE {fprintf(icg,"%d: ifZ %s Goto: %s\n",base++,$<s>6,$<n>1->next->str);} ';' forarg ')' Stmt {fprintf(icg,"%d: %s",base++,$<s>9); fprintf(icg,"\n%d: Goto: %s\n",base++,$<n>1->str); fprintf(icg,"%d: %s:\n",base++,$<n>1->next->str);}
	;

forlabel: FOR {$<n>$=genlabel();}
;

forarg:	| INC ID			{$<s>$=genfor($<s>2,$<s>2,"+","1");}
	| DEC ID	%prec INC	{$<s>$=genfor($<s>2,$<s>2,"-","1");}
	| ID INC	%prec DEC	{$<s>$=genfor($<s>1,$<s>1,"+","1");}
	| ID DEC			{$<s>$=genfor($<s>1,$<s>1,"-","1");}
	| ID '=' forvar '+' forvar	{$<s>$=genfor($<s>1,$<s>3,"+",$<s>5);}
	| ID '=' forvar '-' forvar	{$<s>$=genfor($<s>1,$<s>3,"-",$<s>5);}
	| ID '=' forvar '*' forvar	{$<s>$=genfor($<s>1,$<s>3,"*",$<s>5);}
	| ID '=' forvar '/' forvar	{$<s>$=genfor($<s>1,$<s>3,"/",$<s>5);}
;

forvar: ID	{$<s>$=$1;}
|	NUM	{$<s>$=$1;}
;

FE:	
|	Expr
;

DoWhileStmt: dolabel {fprintf(icg,"\n%d: %s:\n",base++,$<n>1->str);} 
		Stmt 
		WHILE 
		'(' 
		Expr {fprintf(icg,"%d: ifZ %s Goto: %s ",base++,$<s>6,$<n>1->next->str); $<n>1->next->pos=ftell(icg);  fprintf(icg,"___\n"); fprintf(icg,"\n%d: Goto: %s: %d\n",base++,$<n>1->str,$<n>1->addr);}
		')' 
		';' {fprintf(icg,"%d: %s:\n",base++,$<n>1->next->str); $<n>1->next->addr=base-1;
		backpatch($<n>1->next->pos,$<n>1->next);}
;

dolabel: DO	{$<n>$=genlabel();}
;

/* IfStmt Block */
/*
IfStmt : iflabel {printf("\n%s:\n",$<n>1->str);} '(' Expr {printf("ifZ %s Goto: %s\n",$<s>4,$<n>1->next->str);} ')' Stmt {printf("%s:\n",$<n>1->next->str);}	%prec IFX*/
IfStmt : iflabel {fprintf(icg,"\n%d: %s:\n",base++,$<n>1->str);} '(' Expr {fprintf(icg,"%d: ifZ %s Goto: %s\n",base++,$<s>4,$<n>1->next->str);} ')' Stmt {fprintf(icg,"%d: Goto: %s\n",base++,$<n>1->next->next->str);} ELSE {fprintf(icg,"%d: %s:\n",base++,$<n>1->next->str);} Stmt {fprintf(icg,"%d: %s:\n",base++,$<n>1->next->next->str);}
;


iflabel: IF	{$<n>$=genlabelfor();}
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
Expr:	Expr LE Expr 	{$<s>$=gencode($<s>1,"<=",$<s>3);}
	| Expr GE Expr	{$<s>$=gencode($<s>1,">=",$<s>3);}
	| Expr NE Expr	{$<s>$=gencode($<s>1,"!=",$<s>3);}
	| Expr EQ Expr	{$<s>$=gencode($<s>1,"==",$<s>3);}
	| Expr GT Expr	{$<s>$=gencode($<s>1,">",$<s>3);}
	| Expr LT Expr	{$<s>$=gencode($<s>1,"<",$<s>3);}
	| INC ID	{$<s>$=gencode($<s>2,"+","1"); fprintf(icg,"%d: %s = %s\n",base++,$<s>2,$<s>$);}
	| DEC ID	%prec INC	{$<s>$=gencode($<s>2,"-","1"); fprintf(icg,"%d: %s = %s\n",base++,$<s>2,$<s>$);}
	| ID INC	%prec DEC	{$<s>$=gencode($<s>1,"+","1"); fprintf(icg,"%d: %s = %s\n",base++,$<s>2,$<s>$);}
	| ID DEC	{$<s>$=gencode($<s>1,"-","1");  fprintf(icg,"%d: %s = %s\n",base++,$<s>2,$<s>$);}
	| Assignment
	| ArrayUsage
	;
%%
#include"lex.yy.c"
void backpatch(long int labelpos,struct node* n)
{
	long int curr=ftell(icg);
	fseek(icg,labelpos,0);
	fprintf(icg,"%d\n",n->addr);
	fseek(icg,curr,0);	
}
char* gencode(char *p1,char *op,char *p2)
{	
	char tmp[]="_t";
	char res[10];
	sprintf(res,"%d", tmpvar++);
	strcat(tmp,res);
	fprintf(icg,"%d: %s = %s %s %s;\n",base++,tmp,p1,op,p2);
	midresult=(char*)malloc(sizeof(tmp));
	strcpy(midresult,tmp);
	return midresult;
}
struct node* genlabel()
{
	char tmp[]="_L";
	char res[10];
	head=(struct node*)malloc(sizeof(struct node));
	
	sprintf(res,"%d", lblvar++);
	strcat(tmp,res);
	head->str=(char*)malloc(sizeof(tmp));
	strcpy(head->str,tmp);
	head->addr=base;
	head->pos=ftell(icg);

	strcpy(tmp,"_L");
	sprintf(res,"%d", lblvar++);
	strcat(tmp,res);
	head->next=(struct node*)malloc(sizeof(struct node));
	head->next->str=(char*)malloc(sizeof(tmp));
	strcpy(head->next->str,tmp);
	return head;
}
struct node* genlabelfor()
{
	char tmp[]="_L";
	char res[10];
	head=(struct node*)malloc(sizeof(struct node));
	
	sprintf(res,"%d", lblvar++);
	strcat(tmp,res);
	head->str=(char*)malloc(sizeof(tmp));
	strcpy(head->str,tmp);

	strcpy(tmp,"_L");
	sprintf(res,"%d", lblvar++);
	strcat(tmp,res);
	head->next=(struct node*)malloc(sizeof(struct node));
	head->next->str=(char*)malloc(sizeof(tmp));
	strcpy(head->next->str,tmp);

	strcpy(tmp,"_L");
	sprintf(res,"%d", lblvar++);
	strcat(tmp,res);
	head->next->next=(struct node*)malloc(sizeof(struct node));
	head->next->next->str=(char*)malloc(sizeof(tmp));
	strcpy(head->next->next->str,tmp);
	return head;
}
char* genfor(char *lhs,char *p1,char *op,char *p2)
{
	char tmp[50];
	strcpy(tmp,lhs);
	strcat(tmp,"=");
	strcat(tmp,p1);
	strcat(tmp,op);
	strcat(tmp,p2);
	strcat(tmp,";");	
	midresult=(char*)malloc(sizeof(tmp));
	strcpy(midresult,tmp);
	return midresult;
}
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
char* arrdim(char *arr,char *dim)
{
	struct token_list *lexeme,*ptr;
	ptr = header;
	int i;
	for(i=0;i<count;i++,ptr=ptr->next)
	{
		lexeme=ptr;
		if(strcmp(lexeme->name,arr)==0 && lexeme->scope==scope && lexeme->nest==nest)
		{
			//strcpy(lexeme->token_type,"Array");
			//strcpy(lexeme->dimension,dim);
			char tmp1[]="_t";
			char res[10];
			int size;
			sprintf(res,"%d", tmpvar++);
			strcat(tmp1,res);
			if(strcmp(lexeme->dt,"int")==0) size=4;
			else if(strcmp(lexeme->dt,"char")==0) size=1;
			else if(strcmp(lexeme->dt,"float")==0) size=4;
			else if(strcmp(lexeme->dt,"double")==0) size=8;
			fprintf(icg,"%d: %s = %s * %d\n",base++,tmp1,dim,size);
			char tmp2[]="_t";
			sprintf(res,"%d", tmpvar++);
			strcat(tmp2,res);
			fprintf(icg,"%d: %s = %s + %s\n",base++,tmp2,arr,tmp1);
			midresult=(char*)malloc(sizeof(tmp2));
			strcpy(midresult,tmp2);
			return midresult;
		}
	}
}
void arrdimension(char *arr,char *dim)
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
	else  fprintf(icg,"%d: return\n",base++);
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
	else  fprintf(icg,"%d: return %s\n",base++,yytext);
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
			//scope++;
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
	if(type == 'f') temp->scope=0;
	else temp->scope=scope;
	temp->parameter=0;
	temp->next = NULL;
	if(count==0) header=temp;
	else lexeme->next = temp;
	count++;
	if(type == 'f') scope++;
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
		icg = fopen("file.txt","w");
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
		fclose(icg);
		fclose(yyin);
	}
	return 0;
}
         
int yyerror(char *s) {
	printf("Line %d : %s %s\n", yylineno, s, yytext );
}
