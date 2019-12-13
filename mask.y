%{
#include <stdio.h>
#include <string.h>

extern FILE* yyin;
extern int yylineno;
typedef struct expr_info {
	char* nume;
	int intvalue;   //1
        char* strvalue;  //2
	_Bool boolvalue; //3
	char charvalue; //4
	float floatvalue; //5
	int type;
	char* path;
} expr_info;
void init(expr_info* ptr);
void MareaAfisare();
expr_info* master[100];
void update_master(expr_info* expr);

expr_info* create_int_expr(char* name, int value);
expr_info* create_str_expr(char* name, char* value1, char* value2);
expr_info* create_char_expr(char* name, char value);
expr_info* create_bool_expr(char* name, _Bool value1);
expr_info* create_float_expr(char* name, float value);
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
struct expr_info** list_expr_ptr;
}

%token <intval>TIP_CHAR TIP_STRING TIP_INT TIP_BOOL TIP_FLOAT TIP_CLASS IF_CLAUSE FOR_CLAUSE WHILE_CLAUSE  APEL CONSTANT FUN  NR PLUS MINUS MUL DIV  STRLENGTH
//%token <floatval>FLOATT nu avem terminalul FLOATT. Folosim NR ',' NR si construim numarul float in functia de creare_float_expr
%token <boolval>SMALLER EQUAL AND OR BIGGER BOOL 
%token <strval>LITEREE 
%token <charval>LITERA
%token <expr_ptr>STRCPY STRCAT ID_VAR
%type <boolval> conditie  lista_conditii conditie_nr booleana
%type <expr_ptr> declarare atribuire parametru aritmetic numar



%start s
%%

s	: instructiuni {printf("mai sus");}//printf("input acceptat\n"); MareaAfisare();}
	;
instructiuni	: instructiune instructiuni
		| instructiune
		;
instructiune 	: CONSTANT declarare//constante
		| declarare //{printf("mai sus"); update_master($1);}//variabile
		| atribuire
		| clauza //if for while
		| APEL functie //apelarea functiilor predefinite si cele create
		| FUN id_functie '<' '(' lista_declarare ')' '>' '%'instructiuni '%' //creare functii 
		;
declarare	:  TIP_INT '<' '<' ID_VAR ';' {printf("yes");$$ = create_int_expr("max", 0); free($4);}//  //declarare simpla {}
		//|  TIP_FLOAT '<' '<' ID_VAR ';' {printf("%d << %s\n",$1,$<strval>4);}
		|  TIP_CHAR '<' '<' ID_VAR ';' {$$ = create_char_expr($<strval>4, 'a');}
		|  TIP_STRING '<' '<' ID_VAR ';' {$$ = create_str_expr($<strval>4, "a", NULL);}
		|  TIP_BOOL '<' '<' ID_VAR ';'{$$ = create_bool_expr($<strval>4, 0);}
		|  TIP_INT '<' '<' atribuire {$$ = $4;}//declarare + atribuire
		//|  TIP_FLOAT '<' '<' atribuire {$$ = create_float_expr($4);}
		|  TIP_CHAR '<' '<' atribuire {printf("yes");$$ = $4;}
		|  TIP_STRING '<' '<' atribuire {$$ = $4;}
		|  TIP_BOOL '<' '<' atribuire {$$ = $4;}
		//|  TIP_CLASS '<' '<' ID_VAR '#' lista_declarare '#'
		;
lista_declarare	: declarare lista_declarare
		| CONSTANT declarare lista_declarare
	      	| declarare
		| CONSTANT declarare
		;
atribuire	: ID_VAR '<' '-' aritmetic ';'  {$$ = $4;} //numere
		| ID_VAR '<' '-' LITEREE ';'  {expr_info* aux; aux = create_str_expr($<strval>1, $4, NULL); $$ = aux;} //string
		| ID_VAR '<' '-' LITERA ';' {$$ = create_char_expr($<strval>1, $4);}
		| ID_VAR '<' '-' BOOL ';' {$$ = create_bool_expr($<strval>1, $4);}
		//| ID_VAR '<' '-' '%' array '%' ';' //array
		;
lista_atribuiri	: lista_atribuiri atribuire
		| atribuire
		;

		;
/*array	: aritmetic ',' array
	| LITEREE ',' array
	| aritmetic
	| LITEREE {$$ = }
	;
*/
clauza	: IF_CLAUSE '<' '<' lista_conditii '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' lista_declarare ';' lista_conditii ';' lista_atribuiri  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' ';' lista_conditii ';' lista_atribuiri  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' lista_declarare ';' lista_conditii ';'  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' ';' lista_conditii ';'  '>' '>' '%' instructiuni '%'
	| WHILE_CLAUSE '<' '<' lista_conditii '>' '>' '%' instructiuni '%'
	;
lista_conditii	: '(' lista_conditii ')' AND conditie {_Bool x = 0; if($2 && $5) x = 1; $$ = x;}
		| '(' lista_conditii ')' OR conditie {_Bool x = 0; if($2|| $5) x = 1; $$ = x;}
		| conditie {$$ = $1;}
		;
conditie	: booleana
		| conditie_nr
		;
booleana: BOOL {$$ = $1;}
	| ID_VAR {$$ = $1->boolvalue;} 
	;
conditie_nr	: aritmetic BIGGER aritmetic {_Bool x = 0; if($1->intvalue > $3->intvalue) x = 1; $$ = x;}
		| aritmetic SMALLER aritmetic {_Bool x = 0; if($1->intvalue < $3->intvalue) x = 1; $$ = x;}
		| aritmetic EQUAL aritmetic {_Bool x = 0; if($1->intvalue == $3->intvalue) x = 1; $$ = x;}
		;

lista_parametri : lista_parametri ';' parametru
		| parametru
		;
parametru	: NR {$$ = create_int_expr(NULL, $1);}
		| LITERA {$$ = create_char_expr(NULL, $1);}
		| LITEREE {$$ = create_str_expr(NULL, $1, NULL);}
		| ID_VAR {$$ = $1;}
		;
aritmetic	: aritmetic PLUS numar {$$=create_int_expr(NULL, $1->intvalue + $3->intvalue); free($1); free($3);}
		| aritmetic MINUS numar {$$=create_int_expr(NULL, $1->intvalue - $3->intvalue); free($1); free($3);}
		| aritmetic MUL numar {$$=create_int_expr(NULL, $1->intvalue * $3->intvalue); free($1); free($3);}
		| aritmetic DIV numar {$$=create_int_expr(NULL, $1->intvalue / $3->intvalue); free($1); free($3);}
		| numar {$$=create_int_expr(NULL, $1->intvalue); free($1);} //printf("%d\n",$1);}
		;
numar	: NR {$$ = create_int_expr(NULL, $1);}
	|ID_VAR {$$ = $1;}
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
expr_info* create_int_expr(char* name, int value)
{
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   init(expr);
   expr->intvalue = value;
   expr->type = 1;
   strcpy(expr->nume, name); 
   return expr;
}

expr_info* create_str_expr(char* name, char* value1, char* value2) 
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
   strcpy(expr->nume, name); 
   return expr;
		
}

expr_info* create_bool_expr(char* name, _Bool value)
{  
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   expr->boolvalue = value;
   expr->type = 3;
   strcpy(expr->nume, name); 
   return expr;
}

expr_info* create_char_expr(char* name, char value)
{
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   expr->charvalue = value;
   expr->type = 4;
   strcpy(expr->nume, name); 
   return expr;
}

expr_info* create_float_expr(char* name, float value)
{
   expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
   expr->floatvalue = value;
   expr->type = 5;
   strcpy(expr->nume, name); 
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
			printf("Str expr with value: %d ", expr->boolvalue);	
		 }else{
			if(expr->type == 4){
				printf("Str expr with value: %c", expr->charvalue);	
			 }else{
				printf("Str expr with value: %f", expr->floatvalue);
			}
		}	
	}
   }
}
void update_master(expr_info* expr){
	int exista = 0;	
	for(int i = 0; i<99; i++){
		if( strstr(master[i]->nume,expr->nume)!=NULL ){
			exista = 1; // exista si fac update
			master[i]->intvalue = expr->intvalue;   //1
			strcpy(master[i]->strvalue, expr->strvalue);  //2
			master[i]->boolvalue = expr->boolvalue; //3
			master[i]->charvalue = expr->charvalue; //4
			master[i]->floatvalue = expr->floatvalue; //5
			break;		
		}
	}
	if(exista == 0){//nu exista deci il pun
		for(int i = 0; i<99; i++){
			if(master[i]->nume = NULL){
				strcpy(master[i]->nume, expr->nume);
				master[i]->intvalue = expr->intvalue;   //1
				strcpy(master[i]->strvalue, expr->strvalue);  //2
				master[i]->boolvalue = expr->boolvalue; //3
				master[i]->charvalue = expr->charvalue; //4
				master[i]->floatvalue = expr->floatvalue; //5
				master[i]->type = expr->type;
				master[i]->path = expr->path;
				break;
			}
		}	
	}
}
void MareaAfisare(){
	for(int i = 0; i<99; i++)
		if(master[i]->nume != NULL){
			switch(master[i]->type){
				case 1:
					printf("Variabila cu numele %s si tipul int are valoarea %d", master[i]->nume, master[i]->intvalue);break;
				case 2:
					printf("Variabila cu numele %s si tipul string are valoarea %s", master[i]->nume, master[i]->strvalue);break;
				case 3:
					printf("Variabila cu numele %s si tipul bool are valoarea %d", master[i]->nume, master[i]->boolvalue);break;
				//case 4:
					//printf("Variabila cu numele %s si tipul float are valoarea %f", master[i]->nume, master[i]->floatvalue);break;
				default:
					printf("Variabila cu numele %s si tipul char are valoarea %c", master[i]->nume, master[i]->charvalue);break;
			}		
		}
}
void init(expr_info* ptr){
	ptr->nume = NULL;
	ptr->intvalue = 0;   //1
	ptr->strvalue = NULL;  //2
	ptr->boolvalue = 0; //3
	ptr->charvalue = 0; //4
	ptr->floatvalue = 0; //5
	ptr->type = 0;
	ptr->path = 0;
	ptr->path = NULL;
	}
int yyerror(char * s){
 printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
 yyin=fopen(argv[1],"r");
 yyparse();
}

