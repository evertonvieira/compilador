%{
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
extern int yylex ();
extern int yyparse(); 
extern void yyerror(const char* s);
 
#define NADA		9999
#define FRACASSO	9998
#define ACHOUDIFVAR	9997

char *msg4 = "unknow entity in source program";

typedef enum {
    Variable,
    Constant,
    Temporary,
    Function,
    Procedure
} Entity;

/*
    SymbTab: 1as 50 entradas p/ simbolos do fonte
    e últimas p/ as temporarias
*/
typedef struct {
    char     asciiOfSource [20];
    Entity   entt;
    int      value;
} SymbTab;

SymbTab symbTab [100];

int	indSymb,  
	indTemp;

int	topTab=0;   // first 50 entries are programmer symbols
int	topTemp=50; // last  50 entries are temporary

int searchSymbTab (char *symb){ 
    int k;
    for (k = 0; k < topTab; k++)
    if (strcmp(symb,symbTab[k].asciiOfSource) == 0)
        return k;
    return topTab;
};

int insertSymbTab (char *symb, Entity whichEntt) {
    int existingSym, current, aux;

    existingSym = searchSymbTab (symb);
    if (existingSym < topTab) return existingSym;
    current = topTab;
    if ((whichEntt == Variable) || (whichEntt == Constant)) {
        strcpy(symbTab[current].asciiOfSource,symb);
        symbTab[current].entt = whichEntt;
    }
    else {
        char * ptMsg = (char *) malloc (80);
        strcpy(ptMsg,"Unknown entity type: "); 
        strcat(ptMsg,symb); 
        yyerror (ptMsg);
    };
    if (whichEntt == Constant)
        symbTab[current].value = atoi(symb);
    if (whichEntt == Variable) 
        symbTab[current].value = 0;  
    topTab++;
    return current;
};

int temp () { 
	char nomeTemporaria[4];
	int retorno;
    sprintf(nomeTemporaria,"t%d",topTemp-50);
	strcpy(symbTab[topTemp].asciiOfSource,nomeTemporaria);
	symbTab[topTemp].entt = Temporary;
        retorno=topTemp;
	topTemp++;
	return (retorno);
};

void printSymbTable () {
    int i, j, inicio, fimTrecho;
    printf("> Tabela de Simbolos \n");
    inicio=0;
    j=0;
    fimTrecho = topTab-1;// trecho dos s�mbolos do programa  
    while (j <= 1) {
    for (i=inicio; i <= fimTrecho; i++) { 
        switch (symbTab[i].entt) {
            case Variable: printf("> Variable: ");break;
            case Constant: printf("> Numerical Constant: ");break;
            case Temporary: printf("> Temporary: ");break;
            case Function: printf("> Function: ");break;
            case Procedure: printf("> Procedure: ");break;
            default: yyerror(msg4);break;
        };
        printf("%s ", symbTab[i].asciiOfSource);
        printf("%d \n", symbTab[i].value);
    };// do for
    j++;
    inicio = 50;
    fimTrecho=topTemp-1;  // trecho das tempor�rias
    }; // do while
}; // da function printSymbTable

typedef enum {
    ADD,
    SUB,
    MUL,
    DIV,
    STO,
    PRINT,
    JF
} Operador;
char nomeOperador  [6] [7] = {
"ADD","SUB","MUL","DIV","STO","PRINT"};

struct Quadrupla {
	Operador        op;
	int             operando1;
	int             operando2;
	int             operando3;
} quadrupla [ 100 ];

int prox;

void gera (Operador codop,int end1,int end2,int end3){
	quadrupla [prox].op = codop;
	quadrupla [prox].operando1 = end1;
	quadrupla [prox].operando2 = end2;
	quadrupla [prox].operando3 = end3;
	prox++;
};

void remenda (int posM, Operador codop,int end1,int end2,int end3)
{
    quadrupla [posM].op = codop;
    quadrupla [posM].operando1 = end1;
    quadrupla [posM].operando2 = end2;
    quadrupla [posM].operando3 = end3;
};

void imprimeQuadrupla(){
  int r;

  printf("> Tabela de Quadruplas \n");
  for(r=0;r<prox;r++) 
    printf("> %s %d %d %d\n",
              nomeOperador[quadrupla[r].op],                
                 quadrupla[r].operando1,
                    quadrupla[r].operando2,
                       quadrupla[r].operando3);

}; //da funcao imprimeQuadrupla

void finaliza () {
    char resposta [5];
    printSymbTable ();
    imprimeQuadrupla ();
    printf("\n End normal compilation!");
    printf("Para encerrar digite qualquer letra: \n");
    scanf("%s", resposta);
    printf("programa encerrado!\n");
    exit(0);
};

void yyerror(const char *str)
{
  printf("error: %s\n",str);
  exit (1);
};

int yywrap()
{
  return 1;
};

int main()
{
  printf("\n \n>Compilador desenvolvido pelo grupo VII \n>"); 
  yyparse();
  return 0;
}

%}
%union{
    struct T{
        char symbol[21]; 
        int intval;
    }t;
}
%token _ATRIB _EOF _ABREPAR _FECHAPAR _PTVIRG _VIRGULA
%token _BEGIN _END
%token _MAIS _MENOS _MULT _DIVID _PRINT
%token _IF _TINT _ELSE _WHILE _DO
%token _ERRO
%token _N _V
%type<t> E T F _N _V _TINT M N W
%%
/* 
regras da gramatica e acoes semanticas
*/

P : D C
	{
	    finaliza();
	}
	;

D :	L _PTVIRG D
	| L _PTVIRG
	;

L :	T _V 
    {
        insertSymbTab($2.symbol, Variable); 
    }
	| L _VIRGULA _V { insertSymbTab($3.symbol, Variable);}
	;

T :	_TINT
	;

C :	_BEGIN B _END
	;

B : S _PTVIRG B
	| S
	;

S :	A | I | W | O ;

A :	_V _ATRIB E 
	{
		$1.intval = searchSymbTab($1.symbol);
		gera(STO,$3.intval,$1.intval,NADA);
		printf("\n");
    }
	;

I :	_IF E M C 
    {
        remenda($3.intval,JF,prox,NADA, prox);
    }
	| _IF E M C M _ELSE C 
    { 
        remenda($3.intval,JF,$2.intval,$5.intval+1,NADA);
		remenda($5.intval,JF,prox,NADA,NADA);
		printf("\n");
    }
	;

W :	_DO N C _WHILE E
	{ 
        remenda($2.intval,JF,$5.intval,prox,NADA);
		gera(JF, $2.intval,NADA,NADA); 		
		printf("\n");
    }
    ;

O :	_PRINT _ABREPAR E _FECHAPAR
	{ 
        gera(PRINT,$3.intval, NADA, NADA); 
		printf("\n");
    }
	;

E :	E _MAIS T
	{
        $$.intval = temp(); 
		gera(ADD, $1.intval, $3.intval,$$.intval);
    }
	| E _MENOS T
	{
        $$.intval = temp(); 
		gera(SUB, $1.intval, $3.intval,$$.intval);
    }
	| T 
    {
        $$.intval = $1.intval;
    }
    ;

T :	T _MULT F
	{
        $$.intval = temp();
		gera(MUL, $1.intval, $3.intval,$$.intval);
    }
	| T _DIVID F
	{
        $$.intval = temp();
		gera(DIV, $1.intval, $3.intval,$$.intval);
    }
	| F 
    {
        $$.intval = $1.intval;
    }
	;

F :	_PRINT _ABREPAR E _FECHAPAR 
    { 
        gera(PRINT,$3.intval, NADA, NADA); 
		printf("\n");
    }
	| _V 
    {
        $$.intval=insertSymbTab($1.symbol, Variable);
    }
	| _N 
    {
        $$.intval=insertSymbTab($1.symbol, Constant);
    }
	;

M   : /* empty */  
    {
        $$.intval=prox;
		prox++;
	} 
	;

N   :  /* empty */  
    {
        $$.intval=prox;		
    }
	;
%%

void atendeReclamacao () {
  int aux;
  aux = 0; // trying avoid compilation error in bison
}