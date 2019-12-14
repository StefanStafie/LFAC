%{
#include <stdio.h>
#include <string.h>

extern FILE* yyin;
extern int yylineno;
typedef struct expr_info {
	int intvalue;   //1
        char* strvalue;  //2
	_Bool boolvalue; //3
	char charvalue; //4
	float floatvalue; //5
	int type;
	char* path;
} expr_info;


expr_info* master[100];
int k=0;
void update_master(expr_info* expr);
expr_info* search( char* name);
expr_info* create_int_expr(int value);
expr_info* create_str_expr(char* value1, char* value2);
expr_info* create_char_expr(char value);
expr_info* create_bool_expr(_Bool value1);
expr_info* create_float_expr(float value);
void free_expr(expr_info* expr);
void print_expr(expr_info* expr); 


%}
%union {
int intval;
char* strval;
_Bool boolval;
char charval;
float floatval;
struct expr_info* expr_ptr;
}

%token <intval>TIP_CHAR TIP_STRING TIP_INT TIP_BOOL TIP_FLOAT TIP_CLASS IF_CLAUSE FOR_CLAUSE WHILE_CLAUSE  APEL CONSTANT FUN  NR PLUS MINUS MUL DIV  STRLENGTH 
//%token <floatval>FLOATT nu avem terminalul FLOATT. Folosim NR ',' NR si construim numarul float in functia de creare_float_expr
%token <boolval>SMALLER EQUAL AND OR BIGGER BOOL 
%token <strval>LITEREE ID_VAR
%token <charval>LITERA
%token <expr_ptr>STRCPY STRCAT 
%type <boolval> conditie  lista_conditii
%type <expr_ptr>instructiune declarare atribuire parametru 
%type <intval> numeric aritmetic


%start s
%%

s	: instructiuni {printf("input acceptat\n"); printf("%s<=%d",master[0]->path,master[0]->intvalue);} }
	;
instructiuni	: instructiune instructiuni
		| instructiune
		;
instructiune 	: CONSTANT declarare {$$ = create_int_expr(24);} //constante
		| declarare //variabile
		| atribuire		
		| clauza //if for while
		| APEL functie //apelarea functiilor predefinite si cele create
		| FUN id_functie '<' '(' lista_declarare ')' '>' '%'instructiuni '%' //creare functii 
		;
declarare	:  TIP_INT '<' '<' ID_VAR ';' //  //declarare simpla {}
		|  TIP_FLOAT '<' '<' ID_VAR ';' 
		|  TIP_CHAR '<' '<' ID_VAR ';' 
		|  TIP_STRING '<' '<' ID_VAR ';' 
		|  TIP_BOOL '<' '<' ID_VAR ';'
		|  TIP_INT '<' '<' atribuire //declarare + atribuire
		|  TIP_FLOAT '<' '<' atribuire
		|  TIP_CHAR '<' '<' atribuire
		|  TIP_STRING '<' '<' atribuire
		|  TIP_BOOL '<' '<' atribuire
		|  TIP_CLASS '<' '<' ID_VAR '#' lista_declarare '#'
		;
lista_declarare	: declarare lista_declarare
		| CONSTANT declarare lista_declarare
		| declarare
		| CONSTANT declarare
		;
atribuire	: ID_VAR '<' '-' numeric ';' { $$=create_int_expr($4);$$->path=strdup($1);master[k++]=$$; printf("%s<-%d\n",$$->path,$$->intvalue);  } //numeric 
		| ID_VAR '<' '-' LITEREE ';'  //strings
		| ID_VAR '<' '-' LITERA ';'
		| ID_VAR '<' '-' '%' arrayN '%' ';' //array
		| ID_VAR '<' '-' '%' arrayS '%' ';' //array
		| ID_VAR '<' '-' BOOL ';'
		;
lista_atribuiri	: lista_atribuiri atribuire
		| atribuire
		;

		;
arrayN	: numeric ',' arrayN
	| numeric
	;

arrayS 	: LITEREE ',' arrayS	
	| LITEREE	
	;

clauza	: IF_CLAUSE '<' '<' lista_conditii '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' lista_declarare ';' lista_conditii ';' lista_atribuiri  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' ';' lista_conditii ';' lista_atribuiri  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' lista_declarare ';' lista_conditii ';'  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' ';' lista_conditii ';'  '>' '>' '%' instructiuni '%'
	| WHILE_CLAUSE '<' '<' lista_conditii '>' '>' '%' instructiuni '%'
	;
lista_conditii	: '(' lista_conditii ')' AND '(' conditie ')'
		| '(' lista_conditii ')' OR '(' conditie ')'
		| conditie
		;
conditie	: booleana
		| conditie_nr
		;
booleana: BOOL
	| ID_VAR
	;
conditie_nr	: aritmetic BIGGER aritmetic
		| aritmetic SMALLER aritmetic
		| aritmetic EQUAL aritmetic
		;

lista_parametri : lista_parametri ';' parametru
		| parametru
		;
parametru	: NR
		| LITERA
		| LITEREE
		| ID_VAR
		;

numeric : aritmetic {$$=$1;}
	;
aritmetic	: aritmetic PLUS ID_VAR 
		| aritmetic MINUS ID_VAR 
		| aritmetic MUL ID_VAR 
		| aritmetic DIV ID_VAR 
		| aritmetic PLUS NR {$$=$1+$3;}
		| aritmetic MINUS NR {$$=$1-$3;}
		| aritmetic DIV NR {$$=$1/$3;}
		| aritmetic MUL NR {$$=$1*$3;}
		| ID_VAR 
		| NR {$$=$1;}
		;
functie	: id_functie ';' lista_parametri  
	| STRCPY ID_VAR ';' ID_VAR
	| STRCPY ID_VAR ';' LITEREE
	| STRCAT ID_VAR ';' ID_VAR
	| STRCAT ID_VAR ';' LITEREE
	| STRLENGTH ID_VAR
	| STRLENGTH LITEREE
	;
id_functie	: '!' ID_VAR
		;
%%
expr_info* create_int_expr(int value)
{
       
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   expr->intvalue = value;
   expr->type = 1;
   return expr;
}

expr_info* create_str_expr(char* value1, char* value2) 
{
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   int len2 = value2 ? strlen(value2) : 0;
   expr->strvalue = (char*) malloc(sizeof(char)*(strlen(value1) + len2 +1)); 
   strcpy(expr->strvalue, value1);
   if(value2)
   {
      strcat(expr->strvalue, value2);
   }
   expr->type = 2;
   return expr;
		
}

expr_info* create_bool_expr(_Bool value)
{
       
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   expr->boolvalue = value;
   expr->type = 3;
   return expr;
}

expr_info* create_char_expr(char value)
{
       
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   expr->charvalue = value;
   expr->type = 4;
   return expr;
}

expr_info* create_float_expr(float value)
{
       
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   expr->floatvalue = value;
   expr->type = 5;
   return expr;
}

void free_expr(expr_info* expr)
{
  if(expr->type == 2)
  {
     free(expr->strvalue);
  }
  free(expr->path);
  free(expr);
}
void print_expr(expr_info* expr)
{
   if(expr->type == 1) 
   {
	printf("Int expr with value: %d",expr->intvalue);
   }
   else{
	if(expr->type == 2){
		printf("Str expr with value: %s", expr->strvalue);	
	 }else{
		if(expr->type == 3){
			printf("Bool expr with value: %d ", expr->boolvalue);	
		 }else{
			if(expr->type == 4){
				printf("char expr with value: %c", expr->charvalue);	
			 }else{
				printf("float expr with value: %f", expr->floatvalue);
			}
		}	
	}
   }
}

expr_info* search (char * name)
{
	int i=0;
	while (i<=k)
	 {if(master[i]->path==name)
	 	return master[i];
		 i++;
	 }
	 return NULL;

}

int yyerror(char * s){
 printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
 yyin=fopen(argv[1],"r");
 yyparse();
}

