%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

extern FILE* yyin;
extern int yylineno;
typedef struct expr_info {
	_Bool isconst;
	char* nume;
	int intvalue;   //1
        char* strvalue;  //2
	_Bool boolvalue; //3
	char charvalue; //4
	float floatvalue; //5
	int* array; //6
	int arrayn;
	int type;//8 class, 9 function
	char* path;
	int nrParam;
	int paramType[100];
	char* denumiri[100];
	int linieDeclarare;
} expr_info;
typedef struct parametri {
	int nrParam;
	int paramType[100];
	char* denumiri[100];
}parametri;
typedef struct verif {
	char path[256];
	int linie;
	int side;//0 = declarat fara valoare, 1 = declarat cu valoare, 2 = atribuire valoare, 3 = apelare
} verif;

void init(expr_info* ptr);
void MareaAfisare();
char* returneaza_tip(int x);

expr_info* backup[100];
expr_info* master[100];

expr_info* returneaza_daca_exista(char* name);

void remove_from_master(char* nume);
void update_master(expr_info* expr);
void update_path(char* name, char* path);
void update_array(char* name, int pos, int value);
void add_toarray_expr(int value, expr_info* ptr);
void change_array_name(expr_info* ptr, char* name);

int verif_clasa(expr_info* a, char* b);
int verif_apel_functie(expr_info* a, expr_info* b);

parametri* create_lista_declarare(int a, char* nume, int nr, int types[], char* denum[]);
expr_info* create_class_expr(char* name, int nr,int types[], char* denum[]);
expr_info* create_function_expr(char* name, int nr, int types[], char* denum[]);
expr_info* create_array_expr(char* name, int value);
expr_info* create_int_expr(char* name, int value);
expr_info* create_str_expr(char* name, char* value1, char* value2);
expr_info* create_char_expr(char* name, char value);
expr_info* create_bool_expr(char* name, _Bool value1);
expr_info* create_float_expr(char* name, float value, float value2);
char** create_strstr(char** list, char* name);
void free_expr(expr_info* expr);
void print_expr(expr_info* expr); 
void  ItoS ( int value, char* s);
char* ItoS2(int value);
void reverse( char* s );
%}

%union {
int intval;
char* strval;
char strstrval[100][100];
_Bool boolval;
char charval;
float floatval;
struct expr_info* expr_ptr;
struct expr_info** list_expr_ptr;
struct verif* cool;
struct verif* cool_list;
struct parametri* par;
}

	
%token <intval>TIP_CHAR TIP_STRING TIP_INT TIP_BOOL TIP_FLOAT TIP_CLASS IF_CLAUSE FOR_CLAUSE WHILE_CLAUSE  APEL CONSTANT FUN BOOLCPY CHARCPY FLOATCPY NR  STRLENGTH EVAL
%token <boolval>SMALLER EQUAL AND OR BIGGER BOOL 
%token <strval>LITEREE 
%token <charval>LITERA
%token <expr_ptr>STRCPY STRCAT ID_VAR
%type <strval> instructiune id_functie

%type <boolval> conditie  lista_conditii conditie_nr booleana
%type <expr_ptr> declarare atribuire parametru aritmetic_int aritmetic_string aritmetic_char aritmetic_float float_atomic numar caractere array arrayINT functie
%type <strstrval> instructiuni
%type <par> lista_declarare lista_parametri
%start s

%left PLUS MINUS
%left MUL DIV

%%

s	: instructiuni {printf("input acceptat\n");
			for(int i=1;i<100 && $<strstrval>1[i-1] != NULL ;i++) 
				;//printf("%s",$<strstrval>1[i-1]); //marea verificare
			 MareaAfisare();}
	;
instructiuni	: instructiune instructiuni {strcpy($$[0], $<strval>1); for(int i=1, j=0;i<100 && $<strstrval>2[j] != NULL ;i++, j++) if($<strstrval>2[j][0]=='\n') j++; else strcpy($$[i], $<strstrval>2[j]);}
		| instructiune {strcpy($$[0], $<strval>1);}
		;
instructiune 	: CONSTANT declarare {
			if(returneaza_daca_exista($2->nume) != NULL) {printf("LINIA: %d   Variabila cu numele %s exista deja. Nu se poate declara de 2 ori.\n",yylineno, $2->nume); return -1;}
			$2->isconst = 1;  update_master($2); 
			$$ = (char*) malloc(sizeof(char)*(strlen($2->nume) +1)); strcpy($$, $2->nume);}//constante
		| declarare {
			if(returneaza_daca_exista($1->nume) != NULL) {printf("LINIA: %d   Identificatorul %s exista deja. Nu se poate declara de 2 ori.\n",yylineno, $1->nume); return -1;};
			update_master($1); 
			$$ = (char*) malloc(sizeof(char)*(strlen($1->nume) +1));strcpy($$, $1->nume);}//variabile
		| atribuire {
			$$ = (char*) malloc(sizeof(char)*(100));
			if($1!=NULL){
				expr_info* aux = returneaza_daca_exista($1->nume); if(aux == NULL){
					printf("LINIA: %d   Variabila %s nu a fost declarata. Program confuz. Exit\n",yylineno, $1->nume); return -1;}
				update_master($1);
			strcpy($$, $1->nume);} else strcpy($$, "\n");}
		| clauza {$$ = (char*) malloc(sizeof(char)*(100)); strcpy($$, "clauza");}
		| APEL functie APEL {
			expr_info* aux = returneaza_daca_exista($2->nume); if(aux == NULL) {printf("LINIA: %d   Functia %s nu a fost declarata. Program confuz. Exit\n", yylineno,$2->nume); return -1;}
			if(verif_apel_functie(aux, $2) == 0 ) { return -1;}
 			$$ = (char*) malloc(sizeof(char)*(100)); strcpy($$, $2->nume);}
		| EVAL aritmetic_int ';' {printf("Eval linia %d. Rezultatul este: %d\n",yylineno, $2->intvalue); $$ = (char*) malloc(sizeof(char)*(100));strcpy($$, "");}
		;
declarare	:  TIP_INT '<' '<' ID_VAR ';' {$$ = create_int_expr($<strval>4, 0);}//  //declarare simpla {}
		|  TIP_FLOAT '<' '<' ID_VAR ';' {$$ = create_float_expr($<strval>4, 0,0);}
		|  TIP_CHAR '<' '<' ID_VAR ';' {$$ = create_char_expr($<strval>4, 'a');}
		|  TIP_STRING '<' '<' ID_VAR ';' {$$ = create_str_expr($<strval>4, "a", NULL);}
		|  TIP_BOOL '<' '<' ID_VAR ';'{$$ = create_bool_expr($<strval>4, 0);}
		|  TIP_INT '<' '<' atribuire {$$ = $4;}//declarare + atribuire
		|  TIP_FLOAT '<' '<' atribuire {$$ =$4;}
		|  TIP_CHAR '<' '<' atribuire {$$ = $4;}
		|  TIP_STRING '<' '<' atribuire {$$ = $4;}
		|  TIP_BOOL '<' '<' atribuire {$$ = $4;}
		|  TIP_CLASS '<' '<' ID_VAR '#' lista_declarare '#' { $$ = create_class_expr($<strval>4, $6->nrParam, $6->paramType, $6->denumiri);}
		|  FUN id_functie '<' '(' lista_declarare ')' '>' '%' instructiuni '%' { $$ = create_function_expr($<strval>2, $5->nrParam, $5->paramType, $5->denumiri); for(int i=1;i<100 && $<strstrval>9[i-1] != NULL ;i++) {update_path($<strstrval>9[i-1], $<strval>2); remove_from_master($<strstrval>9[i-1]);}}
		;					
lista_declarare	: declarare lista_declarare {$$ = create_lista_declarare($1->type, $1->nume, $2->nrParam, $2-> paramType, $2->denumiri);}
		| declarare { $$ = create_lista_declarare($1->type, $1->nume, 0, NULL, NULL);}
		;
atribuire	: ID_VAR '<' '-' aritmetic_int ';'  { $$ = create_int_expr($<strval>1, $4->intvalue); free($4);}//int
		| ID_VAR '<' '-' aritmetic_string ';'  {expr_info* aux2; aux2 = create_str_expr($<strval>1, $4->strvalue, NULL); $$ = aux2;} //string
		| ID_VAR '<' '-' aritmetic_char ';' {$$ = create_char_expr($<strval>1, $4->charvalue);}//char       
		| ID_VAR '<' '-' lista_conditii ';' {$$ = create_bool_expr($<strval>1, $4);}//booleene
		| ID_VAR '<' '-' aritmetic_float ';' {$$ = create_float_expr($<strval>1, $4->floatvalue, 0);}//float
		| ID_VAR '<' '-' '%' array '%' ';' {expr_info* aux; aux = create_str_expr($<strval>1, $5->strvalue, NULL); $$ = aux;}
		| ID_VAR '<' '-' '#' arrayINT '#' ';' {change_array_name($5,$<strval>1); $$=$5;}
		| ID_VAR '[' aritmetic_int ']' '<' '-' aritmetic_int ';' { update_array($<strval>1,$3->intvalue,$7->intvalue); $$ = create_int_expr($<strval>1, 0);}
		;
lista_atribuiri	: lista_atribuiri atribuire
		| atribuire
		;

aritmetic_float	:aritmetic_float PLUS float_atomic {$$=create_float_expr("", $1->floatvalue + $3->floatvalue, 0); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		|aritmetic_float MINUS float_atomic  {$$=create_float_expr("", $1->floatvalue - $3->floatvalue, 0); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		|aritmetic_float DIV float_atomic  {$$=create_float_expr("", $1->floatvalue / $3->floatvalue, 0); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		|aritmetic_float MUL float_atomic  {$$=create_float_expr("", $1->floatvalue * $3->floatvalue, 0); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		|FLOATCPY float_atomic{$$=create_float_expr("", $2->floatvalue, 0); if($2->nume[0] == '\n') free($2);}
		;
float_atomic	: NR ',' NR {$$=create_float_expr("",$<intval>1, $<intval>3);}
		| ID_VAR {expr_info* aux = returneaza_daca_exista($<strval>1); if(aux == NULL){printf("LINIA: %d   Variabila %s nu a fost declarata. Program confuz. Exit\n",yylineno, $<strval>1); return -1;} if(aux->type != 5) {printf("LINIA: %d   Variabila %s de tip %s trebuia sa aiba tipul float. Program confuz. Exit\n",yylineno,$<strval>1, returneaza_tip(aux->type)); return -1;}$$ = aux;}
		; 
aritmetic_char	: CHARCPY LITERA {$$ = create_char_expr("", $<charval>2);} 
		| CHARCPY ID_VAR {expr_info* aux = returneaza_daca_exista($<strval>2); if(aux == NULL){printf("LINIA: %d   Variabila %s nu a fost declarata. Program confuz. Exit\n", yylineno,$<strval>2); return -1;} if(aux->type != 4) {printf("LINIA: %d   Variabila %s de tip %s trebuia sa aiba tipul char. Program confuz. Exit\n",yylineno,$<strval>2, returneaza_tip(aux->type)); return -1;}$$ = aux;}
		| CHARCPY ID_VAR '[' aritmetic_int ']' {expr_info* aux = returneaza_daca_exista($<strval>2); if(aux == NULL){printf("LINIA: %d   Variabila %s nu a fost declarata. Program confuz. Exit\n", yylineno,$<strval>2); return -1;} if(aux->type != 2) {printf("LINIA: %d   Variabila %s de tip %s trebuia sa aiba tipul string. Program confuz. Exit\n",yylineno,$<strval>2, returneaza_tip(aux->type)); return -1;} $$ = create_char_expr("", aux->strvalue[$4->intvalue]);}
		;
aritmetic_string: aritmetic_string STRCAT caractere {$$ = create_str_expr("", $1->strvalue, $3->strvalue); if($1->nume[0] == '\n') free($1);if($3->nume[0] == '\n') free($3);}
		| STRCPY caractere {$$ = create_str_expr("", $2->strvalue, NULL); if($2->nume[0] == '\n') free($2);}
		;
caractere	:LITEREE {$$ = create_str_expr("", $<strval>1, NULL);}
		|ID_VAR {expr_info* aux = returneaza_daca_exista($<strval>1); if(aux == NULL) {printf("LINIA: %d   Variabila %s nu a fost declarata. Program confuz. Exit\n", yylineno,$<strval>1); return -1;} if(aux->type != 2) {printf("LINIA: %d   Variabila %s de tip %s trebuia sa aiba tipul string. Program confuz. Exit\n", yylineno, $<strval>1, returneaza_tip(aux->type)); return -1;}$$ = aux;}
		;
array	: array '&' aritmetic_int {strcat($1->strvalue, ",");char* s=malloc(sizeof(char*));ItoS($3->intvalue,s); expr_info* aux; aux = create_str_expr("", $1->strvalue, s); $$ = aux;}
	| array '&' LITEREE {strcat($1->strvalue,","); expr_info* aux; aux = create_str_expr("", $1->strvalue, $3); $$= aux;  }
	| aritmetic_int {char* s=malloc(sizeof(char*)); ItoS($1->intvalue,s); expr_info* aux; aux = create_str_expr("", s, NULL); $$ = aux;  }
	| LITEREE  {expr_info* aux; aux = create_str_expr("", $1, NULL); $$ = aux;} 
	;
arrayINT: arrayINT '*' aritmetic_int {add_toarray_expr($3->intvalue,$1); $$=$1; }
	| aritmetic_int{$$=create_array_expr("",$1->intvalue);}
	;

clauza	: IF_CLAUSE '<' '<' lista_conditii '>' '>' '%' instructiuni '%' 
	| FOR_CLAUSE '<' '<' lista_declarare ';' lista_conditii ';' lista_atribuiri  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' ';' lista_conditii ';' lista_atribuiri  '>' '>' '%' instructiuni '%'
	| FOR_CLAUSE '<' '<' lista_declarare ';' lista_conditii ';'  '>' '>' '%' instructiuni '%' 
	| FOR_CLAUSE '<' '<' ';' lista_conditii ';'  '>' '>' '%' instructiuni '%' 
	| WHILE_CLAUSE '<' '<' lista_conditii '>' '>' '%' instructiuni '%' 
	;
lista_conditii	: '(' lista_conditii ')' AND conditie {_Bool x = 0; if($2 && $5 == 1) x = 1; $$ = x;}
		| '(' lista_conditii ')' OR conditie {_Bool x = 0; if($2 || $5 == 1) x = 1; $$ = x;}
		| BOOLCPY conditie {$$ = $2;}
		;
conditie	: booleana {$$ = $1;}
		| conditie_nr {$$ = $1;}
		;
booleana: BOOL {$$ = $1;}
	| ID_VAR {expr_info* aux = returneaza_daca_exista($<strval>1);if(aux == NULL){printf("LINIA: %d    Variabila %s nu a fost declarata. Program confuz. Exit\n",yylineno, $<strval>1); return -1;} if(aux->type != 3) {printf("LINIA: %d    Variabila %s de tip %s trebuia sa aiba tipul bool. Program confuz. Exit\n",yylineno,$<strval>1, returneaza_tip(aux->type)); return -1;}$$ = aux->boolvalue;} 
	;
conditie_nr	: aritmetic_int BIGGER aritmetic_int {_Bool x = 0; if($1->intvalue > $3->intvalue) x = 1; $$ = x;}
		| aritmetic_int SMALLER aritmetic_int {_Bool x = 0; if($1->intvalue < $3->intvalue) x = 1; $$ = x;}
		| aritmetic_int EQUAL aritmetic_int {_Bool x = 0; if($1->intvalue == $3->intvalue) x = 1; $$ = x;}
		;

lista_parametri : parametru ';'lista_parametri {$$->nrParam = 1 + $3->nrParam; $$->paramType[1] = $1->type; for(int i=2; i<=$3->nrParam+2; i++) $$->paramType[i] = $3->paramType[i-1];}
		| parametru {$$->nrParam = 1; $$->paramType[1] = $1->type;}
		;
parametru	: aritmetic_int {$$ = create_int_expr("", $1->intvalue);}
		| aritmetic_string {$$ = create_str_expr("", $1->strvalue, NULL);}
		| aritmetic_float {$$ = create_float_expr("", $1->floatvalue, 0);}
		| aritmetic_char {$$ = create_char_expr("", $1->charvalue);}
		| lista_conditii {$$ = create_bool_expr("", $1);}
		;
aritmetic_int	: aritmetic_int PLUS aritmetic_int {$$=create_int_expr("", $1->intvalue + $3->intvalue); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		| aritmetic_int MINUS aritmetic_int {$$=create_int_expr("", $1->intvalue - $3->intvalue); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		| aritmetic_int MUL aritmetic_int {$$=create_int_expr("", $1->intvalue * $3->intvalue); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		| aritmetic_int DIV aritmetic_int {$$=create_int_expr("", $1->intvalue / $3->intvalue); if($1->nume[0] == '\n') free($1); if($3->nume[0] == '\n') free($3);}
		| numar {$$=create_int_expr("", $1->intvalue); if($1->nume[0] == '\n') free($1);}
		;
numar	: NR {$$ = create_int_expr("", $<intval>1);}
	| NR ',' NR {$$ = create_int_expr("", $<intval>1);}
	|ID_VAR {expr_info* aux = returneaza_daca_exista($<strval>1); if(aux == NULL){printf("LINIA: %d    Variabila %s nu a fost declarata. Program confuz. Exit\n",yylineno, $<strval>1); return -1;} if(aux->type == 5){ aux = create_int_expr(aux->nume, (int)aux->floatvalue);}if(aux->type != 1) {printf("LINIA: %d    Variabila %s de tip %s trebuia sa aiba tipul int sau float. Program confuz. Exit\n",yylineno, $<strval>1, returneaza_tip(aux->type)); return -1;} $$ = aux;}
	| APEL functie APEL {$$=create_int_expr("", 0); expr_info* aux = returneaza_daca_exista($2->nume); if(!verif_apel_functie(aux, $2))return -1;}
	| APEL STRLENGTH aritmetic_string APEL {$$=create_int_expr("",strlen($3->strvalue)); free($3); }
	| ID_VAR '.' ID_VAR {expr_info* aux1 = returneaza_daca_exista($<strval>1); if(!verif_clasa(aux1, $<strval>3)){printf("Clasa %s nu are membrul %s\n", $<strval>1, $<strval>3); return -1;}  $$ = create_int_expr("", 0);}
	;

functie	: id_functie ';' lista_parametri  { $$ = create_function_expr($<strval>1, $3->nrParam, $3->paramType, NULL);} 
	;
id_functie	: '!' ID_VAR {$$ =(char*) malloc(sizeof(char*)*100); strcpy($$, $<strval>2);}
		;
%%
char** create_strstr(char** list, char* name)
{
	if(list == NULL){
		char** aux = (char**)malloc(sizeof(char*)*100);
		aux[0] = (char*) malloc(sizeof(char)*(strlen(name) +1));
		strcpy(aux[0], name);
		return aux;
	}else{
	int i;
	for(i =0; i<100 && list[i] != NULL; i++)
		i++;
	list[i] = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(list[i], name);
	return list;
	}
}
expr_info* create_int_expr(char* name, int value)
{
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	expr->intvalue = value;
	expr->type = 1;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(expr->nume, name); 
	expr->linieDeclarare = yylineno;
	return expr;
}

expr_info* create_str_expr(char* name, char* value1, char* value2) 
{
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	int len2 = value2 ? strlen(value2) : 0;
	expr->strvalue = (char*) malloc(sizeof(char)*(strlen(value1) + len2 +1)); 
	strcpy(expr->strvalue, value1);
	if(value2)
	{
	strcat(expr->strvalue, value2);
	}
	expr->type = 2;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(expr->nume, name); 
	expr->linieDeclarare = yylineno;
	return expr;
		
}

expr_info* create_bool_expr(char* name, _Bool value)
{  
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	expr->boolvalue = value;
	expr->type = 3;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(expr->nume, name);
	expr->linieDeclarare = yylineno; 
	return expr;
}

expr_info* create_char_expr(char* name, char value)
{
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	expr->charvalue = value;
	expr->type = 4;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(expr->nume, name); 
	expr->linieDeclarare = yylineno;
	return expr;
}

expr_info* create_float_expr(char* name, float value, float value2)
{
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	while(value2>1)
		value2=value2/10;
	expr->floatvalue = value+value2;
		//printf("<<%.3f>>",expr->floatvalue);
	expr->type = 5;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(expr->nume, name); 
	expr->linieDeclarare = yylineno;
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
void remove_from_master(char* nume){
	for(int i = 0; i<=99; i++){
		if( master[i] != NULL  && nume != NULL && strstr(master[i]->nume, nume) && strlen(master[i]->nume) == strlen(nume)){
			for(int j = 0; j<99; j++){//caut pozitie in backup
				if(backup[j] == NULL){
					backup[j] = (expr_info*)malloc(9999);
					backup[j]->nume = (char*) malloc(sizeof(char)*(strlen(master[i]->nume) +1));
					strcpy(backup[j]->nume, master[i]->nume);
					backup[j]->intvalue = master[i]->intvalue;   //1
					if(master[i]->strvalue!=NULL) {
						backup[i]->strvalue = (char*) malloc(sizeof(char)*(strlen(master[i]->strvalue) +1));
						strcpy(backup[j]->strvalue, master[i]->strvalue);
					}  //2
					backup[j]->boolvalue = master[i]->boolvalue; //3
					backup[j]->charvalue = master[i]->charvalue; //4
					backup[j]->floatvalue = master[i]->floatvalue; //5
					if(master[i]->array!=NULL) 
						{ 
						backup[j]->array= (int * )malloc (sizeof(int)*10);
						backup[j]->array=master[i]->array; }
					backup[j]->arrayn=master[i]->arrayn;
					backup[j]->type = master[i]->type;
					backup[j]->path = master[i]->path;
					backup[j]->isconst = master[i]->isconst;
					backup[j]->nrParam = master[i]->nrParam;
					for(int	k=1; k<=backup[j]->nrParam; k++)
						backup[j]->paramType[k] = master[i]->paramType[k];
					if(backup[j]->type == 8)	
					for(int	k=1; k<=backup[j]->nrParam; k++){
						backup[j]->denumiri[k] = (char*) malloc(sizeof(char)*(strlen(master[i]->denumiri[k]) +1));
						strcpy(backup[j]->denumiri[k], master[i]->denumiri[k]);
					}
					master[i] = NULL;
					break;
				}
			}		
		}
	}
}
void update_master(expr_info* expr){
	int exista = 0;	
	if(expr == NULL || expr->nume[0] == '\n' || ( strstr(expr->nume, "clauza") && strlen(expr->nume) == 6) || strlen(expr->nume) == 0)
		return;
	for(int i = 0; i<=99; i++){
		if( master[i] != NULL  && expr->nume != NULL && strstr(master[i]->nume, expr->nume) && strlen(master[i]->nume) == strlen(expr->nume)){
			exista = 1; // exista si fac update
			if(master[i]->isconst == 1)
				{printf("Constanta %s nu poate fi modificata", master[i]->nume); return;}
			master[i]->intvalue = expr->intvalue;//1
			if(expr->strvalue!=NULL) {
					master[i]->strvalue = (char*) malloc(sizeof(char)*(strlen(expr->strvalue) +1));
					strcpy(master[i]->strvalue, expr->strvalue);
				}  //2
			master[i]->boolvalue = expr->boolvalue; //3
			master[i]->charvalue = expr->charvalue; //4
			master[i]->floatvalue = expr->floatvalue; //5
			if(expr->array!=NULL) 
					{ 
					master[i]->array= (int * )malloc (sizeof(int)*10);
					master[i]->array=expr->array; }
			master[i]->arrayn=expr->arrayn;
			master[i]->isconst = expr->isconst;
			master[i]->nrParam = expr->nrParam;
			for(int	j=1; j<=master[i]->nrParam; j++)
				master[i]->paramType[j] = expr->paramType[j]; 
			if(master[i]->type == 8)	
				for(int	j=1; j<=master[i]->nrParam; j++){
					master[i]->denumiri[j] = (char*) malloc(sizeof(char)*(strlen(expr->denumiri[j]) +1));
					strcpy(master[i]->denumiri[j], expr->denumiri[j]);
				}
			break;		
		}
	}
	if(exista == 0){//nu exista deci il pun
		for(int i = 0; i<99; i++){
			if(master[i] == NULL){
				master[i] = (expr_info*)malloc(9999);
				master[i]->nume = (char*) malloc(sizeof(char)*(strlen(expr->nume) +1));
				strcpy(master[i]->nume, expr->nume);
				master[i]->intvalue = expr->intvalue;   //1
				if(expr->strvalue!=NULL) {
					master[i]->strvalue = (char*) malloc(sizeof(char)*(strlen(expr->strvalue) +1));
					strcpy(master[i]->strvalue, expr->strvalue);
				}  //2
				master[i]->boolvalue = expr->boolvalue; //3
				master[i]->charvalue = expr->charvalue; //4
				master[i]->floatvalue = expr->floatvalue; //5
				if(expr->array!=NULL) 
					{ 
					master[i]->array= (int * )malloc (sizeof(int)*10);
					master[i]->array=expr->array; }
				master[i]->arrayn=expr->arrayn;
				master[i]->type = expr->type;
				master[i]->path = expr->path;
				master[i]->isconst = expr->isconst;
				master[i]->nrParam = expr->nrParam;
				for(int	j=1; j<=master[i]->nrParam; j++)
					master[i]->paramType[j] = expr->paramType[j];
				if(master[i]->type == 8)	
				for(int	j=1; j<=master[i]->nrParam; j++){
					master[i]->denumiri[j] = (char*) malloc(sizeof(char)*(strlen(expr->denumiri[j]) +1));
					strcpy(master[i]->denumiri[j], expr->denumiri[j]);
				}
				master[i]->linieDeclarare = expr->linieDeclarare;
				break;
			}
		}	
	}
}
void MareaAfisare(){
	FILE *ptr;
	ptr=fopen("symbol.txt","w");
	
	for(int i = 0; i<99; i++)
		if(master[i]!= NULL){
			if(master[i]->isconst == 1)
				{/*printf("CONSTANTA :"); */fprintf(ptr, "CONSTANTA :");}
			switch(master[i]->type){
				case 1:
					//printf("Variabila cu numele %s si tipul int are valoarea %d  scope:%s\n", master[i]->nume, master[i]->intvalue, master[i]->path);
					fprintf(ptr, "int %s=%d  scope:%s\n",master[i]->nume,master[i]->intvalue, master[i]->path);break;
				case 2:
					//printf("Variabila cu numele %s si tipul string are valoarea %s  scope:%s\n", master[i]->nume, master[i]->strvalue, master[i]->path);
					fprintf(ptr, "string %s=%s  scope:%s\n",master[i]->nume,master[i]->strvalue, master[i]->path);break;
				case 3:
					//printf("Variabila cu numele %s si tipul bool are valoarea %d  scope:%s\n", master[i]->nume, master[i]->boolvalue, master[i]->path);
					fprintf(ptr, "_Bool %s=%d  scope:%s\n",master[i]->nume,master[i]->boolvalue, master[i]->path);break;
				case 5:
					//printf("Variabila cu numele %s si tipul float are valoarea %.3f  scope:%s\n", master[i]->nume, master[i]->floatvalue, master[i]->path);
					fprintf(ptr, "float %s=%.3f  scope:%s\n",master[i]->nume,master[i]->floatvalue, master[i]->path);break;
				case 4:
					//printf("Variabila cu numele %s si tipul char are valoarea %c  scope:%s\n", master[i]->nume, master[i]->charvalue, master[i]->path);
					fprintf(ptr, "char %s=%c  scope:%s\n",master[i]->nume,master[i]->charvalue, master[i]->path);break;
				case 6: 
					//printf( "Variabila u numele %s si tipul int-array are valoarea: ",master[i]->nume);
					for(int j=1 ; j<=master[i]->arrayn;j++) 
						//printf("%d, ",master[i]->array[j]); printf("  scope:%s\n", master[i]->path);
					fprintf(ptr, "%s=[ ",master[i]->nume);
					for(int j=1 ; j<=master[i]->arrayn;j++)
						fprintf(ptr, "%d, ",master[i]->array[j]);
					fprintf(ptr, " ]  scope:%s\n", master[i]->path);
					 break;
				case 8:
					//printf( "Clasa cu numele %s si %d variabile de tipul: ",master[i]->nume, master[i]->nrParam);
					for(int j=1 ; j<=master[i]->nrParam;j++) 
						//printf("%s %s, ", returneaza_tip(master[i]->paramType[j]), master[i]->denumiri[j]); printf("  scope:%s\n", master[i]->path);
					fprintf(ptr, "Class %s :: ",master[i]->nume);
					for(int j=1 ; j<=master[i]->nrParam;j++)
						fprintf(ptr, "%s %s, ", returneaza_tip(master[i]->paramType[j]), master[i]->denumiri[j]);
					fprintf(ptr, "  scope:%s\n", master[i]->path);
					break;
				case 9:
					if(!strstr(master[i]->path, "global")){
						//printf( "Functia cu numele %s  scope:%s\n",master[i]->nume, master[i]->path);
						fprintf(ptr, "%s  scope:%s\n",master[i]->nume, master[i]->path);
						break;
					}	
					//printf( "Functia cu numele %s si %d parametri de tipul: ",master[i]->nume, master[i]->nrParam);
					for(int j=1 ; j<=master[i]->nrParam;j++) 
						//printf("%s, ", returneaza_tip(master[i]->paramType[j])); printf("  scope:%s\n", master[i]->path);
					fprintf(ptr, "Function %s :: ",master[i]->nume);
					for(int j=1 ; j<=master[i]->nrParam;j++)
						fprintf(ptr, "%s, ",returneaza_tip(master[i]->paramType[j]));
					fprintf(ptr, "  scope:%s\n", master[i]->path);
					break;
				default: printf("error");
			}		
		}////////////inainte era pt master global. acum urmeaza cele care sunt mai locale, asa
	for(int i = 0; i<99; i++)
		if(backup[i]!= NULL){
			if(backup[i]->isconst == 1)
				{/*printf("CONSTANTA :");*/ fprintf(ptr, "CONSTANTA :");}
			switch(backup[i]->type){
				case 1:
					//printf("Variabila cu numele %s si tipul int are valoarea %d  scope:%s\n", backup[i]->nume, backup[i]->intvalue, backup[i]->path);
					fprintf(ptr, "int %s=%d  scope:%s\n",backup[i]->nume,backup[i]->intvalue, backup[i]->path);break;
				case 2:
					//printf("Variabila cu numele %s si tipul string are valoarea %s  scope:%s\n", backup[i]->nume, backup[i]->strvalue, backup[i]->path);
					fprintf(ptr, "string %s=%s  scope:%s\n",backup[i]->nume,backup[i]->strvalue, backup[i]->path);break;
				case 3:
					//printf("Variabila cu numele %s si tipul bool are valoarea %d  scope:%s\n", backup[i]->nume, backup[i]->boolvalue, backup[i]->path);
					fprintf(ptr, "_Bool %s=%d  scope:%s\n",backup[i]->nume,backup[i]->boolvalue, backup[i]->path);break;
				case 5:
					//printf("Variabila cu numele %s si tipul float are valoarea %.3f  scope:%s\n", backup[i]->nume, backup[i]->floatvalue, backup[i]->path);
					fprintf(ptr, "float %s=%.3f  scope:%s\n",backup[i]->nume,backup[i]->floatvalue, backup[i]->path);break;
				case 4:
					//printf("Variabila cu numele %s si tipul char are valoarea %c  scope:%s\n", backup[i]->nume, backup[i]->charvalue, backup[i]->path);
					fprintf(ptr, "char %s=%c  scope:%s\n",backup[i]->nume,backup[i]->charvalue, backup[i]->path);break;
				case 6: 
					//printf( "Variabila u numele %s si tipul int-array are valoarea: ",backup[i]->nume);
					for(int j=1 ; j<=backup[i]->arrayn;j++) 
						//printf("%d, ",backup[i]->array[j]); printf("  scope:%s\n", backup[i]->path);
					fprintf(ptr, "%s=[ ",backup[i]->nume);
					for(int j=1 ; j<=backup[i]->arrayn;j++)
						fprintf(ptr, "%d, ",backup[i]->array[j]);
					fprintf(ptr, " ]  scope:%s\n", backup[i]->path);
					 break;
				case 8:
					//printf( "Clasa cu numele %s si %d variabile de tipul: ",backup[i]->nume, backup[i]->nrParam);
					for(int j=1 ; j<=backup[i]->nrParam;j++) 
						//printf("%s %s, ", returneaza_tip(backup[i]->paramType[j]), backup[i]->denumiri[j]); printf("  scope:%s\n", backup[i]->path);
					fprintf(ptr, "Class %s :: ",backup[i]->nume);
					for(int j=1 ; j<=backup[i]->nrParam;j++)
						fprintf(ptr, "%s %s, ", returneaza_tip(backup[i]->paramType[j]), backup[i]->denumiri[j]);
					fprintf(ptr, "  scope:%s\n", backup[i]->path);
					break;
				case 9:
					if(!strstr(backup[i]->path, "global")){
						//printf( "Functia cu numele %s  scope:%s\n",backup[i]->nume, backup[i]->path);
						fprintf(ptr, "%s  scope:%s\n",backup[i]->nume, backup[i]->path);
						break;
					}	
					//printf( "Functia cu numele %s si %d parametri de tipul: ",backup[i]->nume, backup[i]->nrParam);
					for(int j=1 ; j<=backup[i]->nrParam;j++) 
						//printf("%s, ", returneaza_tip(backup[i]->paramType[j])); printf("  scope:%s\n", backup[i]->path);
					fprintf(ptr, "Function %s :: ",backup[i]->nume);
					for(int j=1 ; j<=backup[i]->nrParam;j++)
						fprintf(ptr, "%s, ",returneaza_tip(backup[i]->paramType[j]));
					fprintf(ptr, "  scope:%s\n", backup[i]->path);
					break;
				default: printf("error");
			}		
		
	}
}
void init(expr_info* ptr){
	ptr->isconst = 0;
	ptr->nume = NULL;
	ptr->intvalue = 0;   //1
	ptr->strvalue = NULL;  //2
	ptr->boolvalue = 0; //3
	ptr->charvalue = 0; //4
	ptr->floatvalue = 0; //5
	ptr->array=NULL; //6
	ptr->arrayn=0;
	ptr->type = 0;
	ptr->path = (char*)malloc(sizeof(char)*100);
	strcpy(ptr->path, "global");
	ptr->linieDeclarare = yylineno;
}

void reverse ( char* s )
{
 int j=strlen(s);
 char aux;
	for (int i=0;i<=strlen(s)/2;i++)
	{ 	 aux=s[i];
		 s[i]=s[strlen(s)-1-i];
		 s[strlen(s)-1-i]=aux;

	}
}
void ItoS(int a, char* s )
{
	int i=0;
	while(a)
	{
		s[i++]=a%10+'0';
		a=a/10;
		s[i]=0;
		reverse(s);
	}

}
char* ItoS2(int a){
	int b = a;
	char*s = (char*)malloc(sizeof(char)*100);
	int i=0;
	while(b)
	{
		s[i++]=b%10+'0';
		b=b/10;
	}
	s[i] = 0;
	char c;
	for(int j = 0; j<i/2; j++){
		c = s[j];
		s[j] = s[i-j-1];
		s[i-j-1] = c;		
	}
	s[i] = 0;
	return s;
}
expr_info* create_array_expr(char* name, int value)
{
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	expr->array=(int*)malloc(sizeof(int)*100);
	for(int i = 0; i<100;i++)
		expr->array[i] = 99998765;
	expr->array[++expr->arrayn] = value;
	expr->type = 6;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	expr->linieDeclarare = yylineno;
	strcpy(expr->nume, name); 
	return expr;
}
void add_toarray_expr(int value, expr_info* ptr)
{
	ptr->array[++ptr->arrayn]=value;

}
void change_array_name(expr_info* ptr, char* name)

{
	ptr->nume=(char*)malloc(sizeof(char)*(strlen(name)+1));
	strcpy(ptr->nume, name); 
}


void update_array(char* name, int pos, int value )
{
	for(int i=0; i<=99; i++)
		if(master[i]!=NULL && strstr(master[i]->nume,name) && strlen(master[i]->nume) == strlen(name))
			{ master[i]->array[pos]=value; }
}


expr_info* returneaza_daca_exista(char* name){
	for(int i = 0; i<=99; i++){
		if( master[i] != NULL && strstr(master[i]->nume, name) && strlen(master[i]->nume) == strlen(name)){
			return master[i]; 	
		}
	}
	return NULL;
}
char* returneaza_tip(int x){
	switch(x%10){
		case 1:
			return "int";
		case 2:
			return "string";
		case 3:
			return "bool";
		case 5:
			return "float";
		case 4:
			return "char";
		case 6: 
			return "int-array";
		case 9:
			return "function";
		case 8:
			return "class";
		default: return "unknown";
	}	
}
void update_path(char* name, char* path){
	for(int i=0; i<=99; i++)
		if(master[i]!=NULL && strstr(master[i]->nume,name) && strlen(master[i]->nume) == strlen(name)){ 
			master[i]->path = (char*)malloc((sizeof(char))*256);
			strcpy(master[i]->path, path);	break;	
		}
}
int verif_apel_functie(expr_info* a, expr_info* b){
	if(a==NULL || b == NULL)
		{printf("Functia %s nu a fost definita.\n", b->nume);return 0;}
	if(a->type != 9)
		{printf("%s nu este functie\n", a->nume);return 0;}
	if(a->nrParam != b->nrParam)
		{printf("LINIA: %d   Functia %s trebuia sa aiba %d parametri. Program confuz. Exit\n", yylineno, b->nume, a->nrParam);return 0;}
	for(int i = 1; i<= a->nrParam; i++)
		if(a->paramType[i] != b->paramType[i])		
			{printf("LINIA: %d   Functia %s trebuia sa aiba parametrul %d de tipul %s. Program confuz. Exit\n", yylineno, b->nume, i, returneaza_tip(a->paramType[i])); return 0;}
	return 1;
		
}
int verif_clasa(expr_info* a, char* b){
	int ok = 0;
	if(a==NULL || b == NULL)
		return 0;
	for(int i=0;i<=a->nrParam;i++)
		if(a->denumiri[i]!= NULL && strstr(a->denumiri[i], b) && strlen(a->denumiri[i]) == strlen(b))
			ok = 1;
	return ok;
}
expr_info* create_function_expr(char* name, int nr, int types[], char* denum[]){
	if(denum != NULL)
	for(int i=1; i<=nr; i++){
		expr_info* aux= (expr_info*)malloc(sizeof(expr_info));
		init(aux);
		aux->nume = (char*)malloc(sizeof(char) * 100);
		strcpy(aux->nume, denum[i]);		
		strcat(aux->nume, " (ca parametru) ");		
		aux->type = types[i];
		strcpy(aux->path, "In function:");
		strcat(aux->path, name);
		update_master(aux);
	}
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	expr->intvalue = 0;
	expr->type = 9;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(expr->nume, name);
	expr->nrParam = nr;
	for(int i=1; i<=nr; i++)
		expr->paramType[i] = types[i];
	expr->linieDeclarare = yylineno; 
	return expr;
}
expr_info* create_class_expr(char* name, int nr, int types[], char* denum[]){
	for(int i=1; i<=nr; i++){
		expr_info* aux= (expr_info*)malloc(sizeof(expr_info));
		init(aux);
		aux->nume = (char*)malloc(sizeof(char) * 100);
		strcpy(aux->nume, name);
		strcat(aux->nume, ".");		
		strcat(aux->nume, denum[i]);
		aux->type = types[i];
		strcpy(aux->path, "In class:");
		strcat(aux->path, name);
		update_master(aux);
	}
	expr_info* expr = (expr_info*)malloc(sizeof(expr_info));
	init(expr);
	expr->intvalue = 0;
	expr->type = 8;
	expr->nume = (char*) malloc(sizeof(char)*(strlen(name) +1));
	strcpy(expr->nume, name);
	expr->nrParam = nr;
	for(int i=1; i<=nr; i++){
		expr->denumiri[i] = (char*) malloc(sizeof(char)*(strlen(denum[i]) +1));
		strcpy(expr->denumiri[i], denum[i]);
	}
	for(int i=1; i<=nr; i++)
		expr->paramType[i] = types[i]; 
	expr->linieDeclarare = yylineno;
	return expr;
}
parametri* create_lista_declarare(int a, char* nume, int nr, int types[], char* denum[]){
	parametri* aux = (parametri*)malloc(sizeof(parametri)); 
	aux->denumiri[1] = (char*)malloc(sizeof(char)*100); 
	aux->nrParam = 1 + nr;	
	aux->paramType[1] = a; 
	strcpy(aux->denumiri[1], nume); 
	for(int i=2; i<=aux->nrParam; i++) {
		aux->denumiri[i] = (char*)malloc(sizeof(char)*100);
		strcpy(aux->denumiri[i], denum[i-1]);		
		aux->paramType[i] = types[i-1];		
	} 
	
	return aux;
}

int yyerror(char * s){
 printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
 yyin=fopen(argv[1],"r");
 yyparse();
}


