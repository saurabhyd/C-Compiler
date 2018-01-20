%{
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>	
	int count=0,nest_cmt=0,line_count=1,comm_line;	
	struct token_list
	{
		int attr_num;
		char *name,token_type[25];		
		struct token_list *next;
	}*header;
	void insert(char *yytext,char type);
%}

/*============================================================================================================================================*/

keyword		"auto"|"break"|"case"|"char"|"const"|"continue"|"default"|"do"|"double"|"else"|"enum"|"extern"|"float"|"for"|"goto"|"if"|"int"|"long"|"register"|"return"|"short"|"signed"|"sizeof"|"static"|"struct"|"switch"|"typedef"|"union"|"unsigned"|"void"|"volatile"|"while"
letter		[a-zA-Z_]
loop		"for"|"while"
id		{letter}({letter}|{digit})*
digit		[0-9]
hexa		[a-fA-F0-9]
oct		[0-7]
exp		[Ee][+-]?{digit}+
float_suff	(f|F|l|L)
int_suff	(u|U|l|L|ul|UL|ll|LL|ull|ULL)
singlecomm 	(\/\/.*)
comm_beg 	(\/\*)
comm_end 	(\*\/)
space 		[ \t]+
punctuator	";"|"{"|"}"|","|":"|"("|")"|"."
operator	">>="|"<<="|"+="|"-="|"*="|"/="|"%="|"&="|"^="|"|="|">>"|"<<"|"++"|"--"|"->"|"&&"|"||"|"<="|">="|"=="|"!="|"&"|"!"|"~"|"-"|"+"|"*"|"/"|"="|"%"|"<"|">"|"^"|"|"|"?"

%x COMM

/*============================================================================================================================================*/

%%

 /* Line Count */
<*>\n	{line_count++;}

 /* Pre-processor directives */
^#([-a-zA-Z0-9.]|<|>|{space})* 	{insert(yytext,'d');}

 /* RegEx for Keywords */
{keyword}  						{ insert(yytext,'k'); }

 /* RegEx for Constants Literals */
0[xX]{hexa}+{int_suff}?				{insert(yytext,'1');}
0{oct}+{int_suff}?				{insert(yytext,'2');}
{digit}+{int_suff}?				{insert(yytext,'3');}
{digit}+{float_suff}?				{insert(yytext,'4');}
{digit}+{exp}{float_suff}?			{insert(yytext,'4');}
{digit}*"."{digit}+({exp})?{float_suff}?	{insert(yytext,'4');}
{digit}+"."{digit}*({exp})?{float_suff}?	{insert(yytext,'4');}

 /*RegEx for Identifier */
{id}			{ insert(yytext,'i'); }
{digit}+{letter}+     	{ printf("Line %d: error: Invalid Identifier\n",line_count);}

 /*RegEx to identify functions and loops*/
{loop}{space}?"("	{insert(yytext,'l');}
{keyword}{space}?"("	{insert(yytext,'k');}
{id}{space}?"("		{insert(yytext,'f');}

		/* {id}{space}?"("(({keyword}{space}"*"?{id}?",")*{keyword}{space}"*"?{id})?")"	{insert(yytext,'f');} */
		/* {id}{space}?"("(("*"?{id}?",")*"*"?{id})?")"	{insert(yytext,'f');}*/


 /*RegEx for Comments*/
{singlecomm}	{}

{comm_beg}	{  	
			BEGIN(COMM);
			nest_cmt++;
			comm_line=line_count;
	        }

{comm_end}	{ printf("Line %d: error: Invalid Comment Terminator\n",line_count);}

<COMM>{comm_beg}	{	
				nest_cmt++;
				if(nest_cmt>1)	printf("Line %d: error: Nested Comment\n",line_count);
               		}

<COMM>{comm_end} 	{
		                if(nest_cmt>0)	nest_cmt--;
		                if(nest_cmt==0)	BEGIN(INITIAL);
               		}

<COMM>. {}

 /* RegEx for String Literals */
\"(\\.|[^\\"])*\"	{insert(yytext,'s');}
\"(\\.|[^\\"])*		{printf("Line %d: error: Incomplete String Literal\n",line_count);}

 /*RegEx for Operators*/
{operator}	{insert(yytext,'o');}

 /*RegEx for Punctuators*/
{punctuator}	{}	
			
 /* RegEx to ignore unnecessary spaces */
[ \t\v\n\f]	{}
"["|"]"|"\'\\0\'" {}

.	{printf("Line %d: error: Bad Character\n",line_count);}

%%

/*============================================================================================================================================*/

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
			if(yytext[strlen(yytext)-1]=='(') yytext[strlen(yytext)-1]='\0';
			break;
		case 'l':
			strcpy(token_type,"Keyword (Loop)");
			yytext[strlen(yytext)-1]='\0';
			break;
		case 'i':
			strcpy(token_type,"Identifier (Variable)");
			break;
		case 'f':
			strcpy(token_type,"Identifier (Function)");
			yytext[strlen(yytext)-1]='\0';
			break;
		case 's':
			strcpy(token_type,"String Literal");
			break;
		case 'o':
			strcpy(token_type,"Operator");
			break;
		case 'd':
			strcpy(token_type,"Preprocessor Directive");
			break;
		case '1':
			strcpy(token_type,"Constant (Hexadecimal)");
			break;
		case '2':
			strcpy(token_type,"Constant (Octal)");
			break;
		case '3':
			strcpy(token_type,"Constant (Integer)");
			break;
		case '4':
			strcpy(token_type,"Constant (Float)");
			break;
	}
	if(nest_cmt==0)
	{
		int i;
		for(i=0;i<count;i++,ptr=ptr->next)
		{
			lexeme = ptr;
			if(strcmp(lexeme->name,yytext)==0) break;
		}
		if(i==count)
		{
			temp = (struct token_list*)malloc(sizeof(struct token_list));
			temp->attr_num=line_count;
			temp->name = (char*)malloc((len1+1)*sizeof(char));
			strcpy(temp->name,yytext);
			strcpy(temp->token_type,token_type);
			temp->next = NULL;
			if(count==0) header=temp;
			else lexeme->next = temp;
			count++;
			fprintf(yyout,"\n%35s %30s %25d",temp->name,temp->token_type,temp->attr_num);
		}
	}
}
                                                                                                           
/* main() function */

int main()
{
	yyin=fopen("test4.txt","r");
	yyout=fopen("out.txt","w");
	fprintf(yyout,"\t\t\t\t\t--------------------------------------------\n");
	fprintf(yyout,"\t\t\t\t\t\t\tSYMBOL TABLE\n");
	fprintf(yyout,"\t\t\t\t\t--------------------------------------------\n");
	fprintf(yyout,"\t\t\tLexeme\t\t\t\tToken\t\t\tAttribute Value\n");
	yylex();
	if(nest_cmt!=0)	printf("Line %d: error: Unterminated Comment\n",comm_line);
	fprintf(yyout,"\n");
	fclose(yyout);
}

int yywrap() {return 1;}
