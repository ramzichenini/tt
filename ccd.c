#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_ID_LENGTH 30
#define LEX_ERROR -2
#define TYPE_ERROR -3
#define VOID_TYPE -4

#define PROGRAM  0
#define VAR 1
#define INTEGER 2
#define CHAR 3
#define BEGIN 4
#define END 5
#define IF 6
#define THEN 7
#define ELSE 8
#define WHILE 9
#define DO 10
#define READ 11
#define READLN 12
#define WRITE 13
#define WRITELN 14

#define ID 15
#define NB 16

#define PV 17
#define P 18
#define DP 19
#define DPE 20
#define V 21
#define PO 22
#define PF 23
#define OPREL 24
#define OPADD 25
#define OPMUL 26


const char * const keywords[] = {"program","var","integer","char","begin","end",
                                 "if","then","else","while","do","read","readln","write","writeln"
                                };
const char * const identify[] = {"'program'","'var'","'integer'","'char'","begin","end",
                                 "'if'","'then'","'else'","'while'","'do'","'read'","'readln'","'write'","'writeln'","'id'","'nb'"
                                 ,"';'","'.'","':'","':='","','","'('","')'","'oprel'","'opadd'","'opmul'"
                                };
typedef struct symb
{
    int ul; /** entre -4 et 26 */
    int att; /**la position dans le tableau des identificateur si c'est un identifiant */
} symb;

typedef struct table_symb /** le tableau des identificateurs */
{
    char id[MAX_ID_LENGTH];
    int type;
    void *ptr;
} table_symb;

FILE *ficEntree;
FILE *trad;
symb symbole;
symb result;
int id_size=5; /** taille initial du tableau d'id */
int id_head=0; /** e5er position t3abbet fel tableau */
table_symb *id_tab;
int l; // contient le nombre de ID dans chaque declaration
int dcl_flag=1;
int new_id_flag=0;
int deb; // flag =1 si on est dans le program, =0 si on a termine le traitement des instructions
int ligne=1;
int er =0; //nb d'erreur



int unilex_id(const char *ch)//retourner le num clé ou num ID //bch tchouf.ha mwjouda fel les mot clé ou nn
{
    int i = 0;
    while((i != 15) && (strcmp(ch,keywords[i]) != 0))
    {
        ++i;
    }
    return i;
}

int ranger_id(int code_ul, const char *ch) /** retourner la position dans le tableau d'ID keno id*/
{
    int i = 0;
    new_id_flag=0; /** egale 0 si le variable est deja déclaré*/

    if (code_ul != ID)  /** si different de 15 */
    {
        return -1;   // ch is keyword
    }
    else
    {
        while((i < id_head) && (strcmp(ch,id_tab[i].id) != 0))
        {
            ++i;
        }
        if(i==id_head)  // le id ne figure pas dans le table d'identif
        {
            new_id_flag=1;
            if(++id_head>id_size)
            {
                id_size*=2; // bhama hhhh
                id_tab = realloc(id_tab, id_size * sizeof(table_symb));
            }
            strcpy(id_tab[i].id, ch);
        }
        return i;
    }
}

symb AnalLex() /** retourner le symbole lexical */
{
    char car = '\0';
    char ch[MAX_ID_LENGTH];
    symb unilex;
    int n = 0;
    int etat=0;
    while (1)
    {
        switch(etat)
        {
        case 0:
            car = getc(ficEntree);
            if (car == ' ' || car == '\t') etat = 0;
            else if(car=='\n')
            {
                ligne++; // erreur een ligne **
                etat=0;
            }
            else if (isalpha(car))
            {
                etat = 1;
                ch[n++]=car;
            }
            else if (isdigit(car))
            {
                etat = 3;
                ch[n++] = car;
            }
            else if (car == ';') etat = 5;
            else if (car == '.') etat = 6;
            else if (car == ':') etat = 7;
            else if (car == ',') etat = 10;
            else if (car == '(') etat = 11;
            else if (car == ')') etat = 15;
            else if (car == '<') etat = 16;
            else if (car == '>') etat = 20;
            else if (car == '=') etat = 23;
            else if (car == '+') etat = 25;
            else if (car == '-') etat = 26;
            else if (car == '|') etat = 27;
            else if (car == '*') etat = 29;
            else if (car == '/') etat = 30;
            else if (car == '%') etat = 31;
            else if (car == '&') etat = 32;
            else if (car == EOF) etat = 34;
            else
            {
                result.ul = LEX_ERROR;
                return result ;
            }
            break;

        case 1 :
            car = getc(ficEntree);
            if(isalnum(car))
            {
                if(n < MAX_ID_LENGTH)
                    ch[n++]=car;
            }
            else
            {
                etat = 2;
            }
            break;
        case 2 :
            ungetc(car,ficEntree); // b"ch trajja3 el autre
            ch[n]='\0';
            result.ul  = unilex_id(ch); /*** entre 0 et 15 ***/
            result.att = ranger_id(result.ul, ch);
            return result;

        case 3 :
            car = getc(ficEntree);
            if(isdigit(car))
            {
                if(n < MAX_ID_LENGTH)
                    ch[n++]=car;
            }
            else
            {
                etat = 4;
            }
            break;
        case 4 :
            ungetc(car,ficEntree);
            ch[n]='\0';
            result.ul = NB;

            return result;
        case 5 :
            result.ul=PV;
            return result;
        case 6 :
            result.ul=P;
            return result;
        case 7 :
            car = getc(ficEntree);
            if( car == '=')
            {
                etat = 8;
            }
            else
            {
                etat = 9;
            }
            break;
        case 8 :
            result.ul = DPE ;
            return result;
        case 9 :
            ungetc(car,ficEntree);
            result.ul=DP;
            return result;

        case 10 :
            result.ul=V;
            return result;
        case 11 :
            car = getc(ficEntree);
            if (car == '*')
            {
                etat = 12;
            }
            else
            {
                etat = 14;
            }
            break;
        case 12 :
            car=getc(ficEntree);
            if(car=='*')
            {
                etat=13;
            }
            break;
        case 13 :
            car = getc(ficEntree);
            if (car == ')')
            {
                etat = 0;
            }
            else
            {
                etat = 12;
            }
            break;
        case 14 :
            ungetc(car,ficEntree);
            result.ul = PO;
            return result;
        case 15 :
            result.ul =PF;
            return result;
        case 16 :
            car = getc(ficEntree);
            if (car == '=')
            {
                etat = 17;
            }
            else if (car=='>')
            {
                etat=18;
            }
            else etat=19;
            break;
        case 17 :
            result.ul = OPREL;
            return result;
        case 18 :
            result.ul = OPREL;
            return result;
        case 19 :
            ungetc(car,ficEntree);
            result.ul = OPREL;
            return result;

        case 20 :
            car = getc(ficEntree);
            if (car == '=')
            {
                etat = 21;
            }
            else etat=22;
            break;

        case 21 :
            result.ul = OPREL;
            return result;
        case 22 :
            ungetc(car,ficEntree);
            result.ul = OPREL;
            return result;

        case 23 :
            car = getc(ficEntree);
            if(car == '=')
            {
                etat = 24 ;
            }
            else
            {
                result.ul = LEX_ERROR;
                return result ;
            }
            break;
        case 24 :
            result.ul = OPREL;
            return result;

        // OPADD
        case 25 :
            result.ul = OPADD;
            return result;
        case 26 :
            result.ul = OPADD;
            return result;
        case 27 :
            car = getc(ficEntree);
            if( car == '|')
            {
                etat = 28;
            }
            else
            {
                result.ul = LEX_ERROR;
                return result ;
            }
            break;
        case 28 :
            result.ul = OPADD;
            return result;

        // OPMUL
        case 29 :
            result.ul = OPMUL;
            return result ;
        case 30 :
            result.ul = OPMUL;
            return result ;
        case 31 :
            result.ul = OPMUL;
            return result ;
        case 32 :
            car = getc(ficEntree);
            if( car == '&')
            {
                etat = 33;
            }
            else
            {
                result.ul = LEX_ERROR;
                return result ;
            }
            break;
        case 33 :
            result.ul = OPMUL;
            return result ;
        // EOF
        case 34 :
            result.ul = EOF ;
            return result;
        };
    }
}

int accepter(int t) /** si ID retourne position dans table id, si mot cle ne retourne rien, sinon retourne erreur*/
{
    int id_num;

    if(symbole.ul==t)
    {
        if(symbole.ul==ID)  /**  partie de l'Analyseur sémantique */
        {
            id_num=symbole.att;
            if((new_id_flag)&&(!dcl_flag)) // kén fotna el declaration w a7na ltaaw la declarinéh ngolou raw errrr
            {
                er++;
                printf("ligne %d :ERREUR Semantique: %s non declaree\n",ligne,id_tab[id_head-1].id);
            }
            if((!new_id_flag)&&(dcl_flag))
            {
                er++;
                printf("ligne %d :ERREUR Semantique: %s deja declaree\n",ligne,id_tab[id_head-1].id);
            }
        }
        symbole=AnalLex();
        return id_num;
    }
    else
    {
        er++;
        if(symbole.ul==LEX_ERROR)
        {
            printf("ligne %d :ERREUR lexical\n",ligne);
        }
        else
            printf("ligne %d: ERREUR syntaxique : attendu %s mais trouve %s \n",ligne,identify[t],identify[symbole.ul]);

        return TYPE_ERROR;
    }
}
int facteur()
{
    int indice,t2;
    if (symbole.ul==ID)
    {
        indice=accepter(ID);
        t2=id_tab[indice].type;
        fprintf(trad,"valeurd %p\n",id_tab[indice].ptr);
        return t2 ;
    }
    else if (symbole.ul==NB)
    {
        indice=accepter(NB);
        fprintf(trad,"empiler %d\n",indice);
        return INTEGER;
    }
    else if (symbole.ul==PO)
    {
        accepter(PO);
        t2=expr();
        accepter(PF);
        return t2;
    }
    else
    {
        if(deb=1)
        {
            while(1)
            {
                if((symbole.ul==ID)||(symbole.ul==NB)||(symbole.ul==PO)||(symbole.ul==EOF)) break ;
                else {er++;
                        printf("ligne %d :facteur non trouve , attendu : 'id' , 'nb' , '(' \n",ligne);
                symbole=AnalLex();}
            }

            if ((symbole.ul==ID)||(symbole.ul==NB)||(symbole.ul==PO))
            {
                facteur();
            }
        }
        else
        {

            do
            {
                printf("ligne %d: facteur 2 unexpected %s\n",ligne,identify[symbole.ul]);
                symbole=AnalLex();
            }
            while((symbole.ul==OPMUL)||(symbole.ul==OPADD)||(symbole.ul==OPREL)
                  ||(symbole.ul==THEN)||(symbole.ul==DO)||(symbole.ul==PO)||(symbole.ul==EOF));
        }
    }
}

int ter()
{
    int t1,t2;
    if(symbole.ul==OPMUL)
    {
        accepter(OPMUL);
        t1=facteur();
        t2=ter();
        fprintf(trad,"opmul %d\n",symbole.ul);
        if(t2!=VOID_TYPE)
        {
            if(t1!=t2)
                return TYPE_ERROR;
            else return t1;
        }
        else return t1;
    }
    else return VOID_TYPE;
}
int terme()
{
    int t1,t2;
    t1=facteur();
    t2=ter();
    if(t2 != VOID_TYPE)
    {
        if(t1==t2)
            return t1;
        else return TYPE_ERROR;
    }
    else return t1;

}
int exp_sim()
{
    int t1,t2;
    if(symbole.ul==OPADD)
    {
        accepter(OPADD);
        t1=terme();
        t2=exp_sim();
        fprintf(trad,"opadd %d\n",symbole.ul);
        if(t2!=VOID_TYPE)
        {
            if(t1!=t2)
                return TYPE_ERROR;
            else return t1;
        }
        else return t1;
    }
    else return VOID_TYPE;
}
int exp_simple()
{
    int t1,t2;
    t1=terme();
    t2=exp_sim();
    if(t2 != VOID_TYPE)
    {
        if(t1==t2)
            return t1;
        else return TYPE_ERROR;
    }
    else return t1;
}
int s()
{
    int type;
    if(symbole.ul==OPREL)
    {
        accepter(OPREL);
        type=exp_simple();
        fprintf(trad,"oprel %d\n",symbole.ul);
        return type;
    }
    else
        return VOID_TYPE;
}
int expr()
{
    int t1,t2;
    t1=exp_simple();
    t2=s();
    if(t2 != VOID_TYPE)
    {
        if(t1==t2)
        {
            return t1;
        }
        else
        {
            return TYPE_ERROR;
        }
    }
    else
        return t1;

}
void i() /**instrcution */
{
    int t1,t2,indice;
    if (symbole.ul==ID)
    {
        indice=accepter(ID);
        fprintf(trad,"valeurg %p\n",id_tab[indice].ptr);
        t1=id_tab[indice].type;
        accepter(DPE);
        t2=exp_simple();
         fprintf(trad,":=\n");
        if(t1!=t2)
        {
            er++;
            printf("ligne %d:  ERREUR Semantique : le type est non valide\n",ligne);
        }
    }
    else if(symbole.ul==IF)
    {
        accepter(IF);
        if(expr()==TYPE_ERROR)
        {
            er++;
            printf("ligne %d :ERREUR Semantique :pas d'equivalance de type \n",ligne);
        }
        fprintf(trad,"aller si faux \n");

        accepter(THEN);
        i();
        fprintf(trad,"jump sortie \n");
        accepter(ELSE);
        i();
        fprintf(trad,"jump sortie \n");
    }
    else if(symbole.ul==WHILE)
    {
        int sortie=2,test=1;
        accepter(WHILE);
        fprintf(trad,"Etiq Sortie %d \n",sortie);
        fprintf(trad,"aller a test %d \n");
        if(expr()==TYPE_ERROR)
        {
            er++;
            printf("ligne %d :ERREUR Semantique : pas d'equivalance de type \n",ligne);
        };
        fprintf(trad,"aller si faux Sortie %d\n",sortie);
        accepter(DO);
        i();
        fprintf(trad,"Etiq Test %d \n",test);
    }
    else if (symbole.ul==READ)
    {
        accepter(READ);
        accepter(PO);
        accepter(ID);
        accepter(PF);
    }
    else if(symbole.ul==READLN)
    {
        accepter(READLN);
        accepter(PO);
        indice=accepter(ID);
        fprintf(trad,"readln valeur %d \n",id_tab[indice].ptr);
        accepter(PF);
    }
    else if (symbole.ul==WRITE)
    {
        accepter(WRITE);
        accepter(PO);
        indice=accepter(ID);
        fprintf(trad,"write valeur %d \n",id_tab[indice].ptr);
        accepter(PF);
    }
    else if(symbole.ul==WRITELN)
    {
        accepter(WRITELN);
        accepter(PO);
        indice=accepter(ID);
        fprintf(trad,"writeln valeur %d \n",id_tab[indice].ptr);
        accepter(PF);
    }
    else
    {
        if(symbole.ul==END)
        {
            printf("ligne %d :ERREUR Syntaxique : attendu ';' mais non trouve \n",ligne);
        }
        else if(deb=1)
        {
            do
            {
                er++;
                printf("ligne %d : ERREUR Syntaxique en %s attendu 'id','if','while','read','readln','write',writeln'\n",ligne,identify[symbole.ul]);
                symbole=AnalLex();
            }
            while((symbole.ul==ID)||(symbole.ul==IF)||(symbole.ul==WHILE)||(symbole.ul==READ)||
                    (symbole.ul==READLN)||(symbole.ul==WRITE)||(symbole.ul==WRITELN)||(symbole.ul==EOF));
            if ((symbole.ul==ID)||(symbole.ul==IF)||(symbole.ul==WHILE)||(symbole.ul==READ)||
                    (symbole.ul==READLN)||(symbole.ul==WRITE)||(symbole.ul==WRITELN))
            {
                i();
            }
        }
        else
        {
            do
            {
                symbole=AnalLex();
                printf("ligne %d :probleme de fin d'instruction %s\n",ligne,identify[symbole.ul]);
            }
            while((symbole.ul==PV)||(symbole.ul==END)||(symbole.ul==ELSE));
        }
    }

}
void li() /**Liste_inst ' */
{
    if(symbole.ul==PV)
    {
        accepter(PV);
        i();
        li(); /**Liste_inst ' */
    }
    else
    {
        /** epsilon */
    }
}
void liste_inst()
{
    i(); /**instrcution */
    li(); /**Liste_inst ' */
}
void inst_composee()
{
    if(symbole.ul==BEGIN)
    {
        accepter(BEGIN);
        liste_inst();
        accepter(END);
    }
    else
    {

        if(deb=1)
        {
            do
            {
                er++;
                symbole=AnalLex();
                printf("ligne %d :ERROR Syntaxique : Probleme de BEGIN \n",ligne);
            }
            while((symbole.ul==BEGIN)||(symbole.ul==EOF));
            if (symbole.ul==BEGIN)
            {
                inst_composee();
            }
        }
        else
        {
            do
            {
                symbole=AnalLex();
                printf("ligne %d :ERROR Syntaxique : Probleme d'instruction \n",ligne);
            }
            while(symbole.ul==P);
        }
    }
}
int type()
{
    if(symbole.ul==INTEGER)
    {
        accepter(INTEGER);
        return INTEGER;
    }
    else if (symbole.ul==CHAR)
    {
        accepter(CHAR);
        return CHAR;
    }
    else
    {
        if(deb=1)
        {
            do
            {
                er++;
                symbole=AnalLex();
                printf("ligne %d: ERREUR syntaxique : type non trouve \n",ligne);


            }
                while((symbole.ul==INTEGER)||(symbole.ul==CHAR)||(symbole.ul==EOF));
            if ((symbole.ul==INTEGER)||(symbole.ul==CHAR))
            {
                type();
            }
        }
        else
        {
            while(symbole.ul==PV)
            {
                symbole=AnalLex();
                printf("ligne %d: ERREUR syntaxique : probleme de type \n",ligne);

            }

        }
    }
}
int ld() /** list_ad' */
{
    if (symbole.ul==V)
    {
        accepter(V);
        accepter(ID);
        l++;
        ld();
    }
    else return l++;
}
int liste_id()
{
    if (symbole.ul==ID)
    {   l=0;
        accepter(ID);
        ld(); /** list_ad' */
        return(ld());
    }
    else
    {
        do
        {
            er++;
            symbole=AnalLex();
            printf("ligne %d: ERREUR syntaxique : identifiant non trouve \n",ligne);

        }
        while((symbole.ul==ID)||(symbole.ul==EOF));


        if (symbole.ul==ID)
        {
            liste_id();
        }
    }
}
void dcl()
{
    int t,i;
    dcl_flag=1;
    if(symbole.ul == VAR)
    {
        accepter(VAR);
        l=liste_id();
        accepter(DP);

        t=type();

        for(i=0; i!=l; ++i)
        {
            id_tab[id_head-i-1].type=t;
            if(t==INTEGER)
            {
                id_tab[id_head-i-1].ptr=malloc(sizeof(int));
            }
            else if(t==CHAR)
            {
                id_tab[id_head-i-1].ptr=malloc(sizeof(char));
            }
        }
        accepter(PV);
        dcl();
    }
    else
    {
        //epsilon
    }
}
void p()
{

    if (symbole.ul == PROGRAM)
    {
        accepter(PROGRAM);
        accepter(ID);
        accepter(PV);
        dcl_flag=1; /** on est avant la declaration */
        dcl();
        dcl_flag=0; /** on est apres la declaration */
        inst_composee();
        deb=0;
        accepter(P);

    }
    else
    {
        do
        {
            er++; // nbr err
            symbole=AnalLex();
            printf("Le mot 'program' non trouve \n ");
        }
        while((symbole.ul== PROGRAM)||(symbole.ul==EOF));

        if ((symbole.ul== PROGRAM))
            p();
        else
            printf("Le mot 'program' non trouve \n ");
        }
    }



int main()
{
    ficEntree = fopen("test.txt", "r");
    trad=fopen("traduc.txt","w");
    id_tab = malloc(id_size * sizeof(table_symb));
    symbole=AnalLex();
    deb=1;
    p();
    printf("compilation fini  : %d erreur(s) sont trouve(s)",er);
    fclose(ficEntree);
    fclose(trad);
    int i;
        for(i=0;i!=id_head;++i){
    free(id_tab[id_head-i-1].ptr);
        }
    free(id_tab);
    return 1;
}
