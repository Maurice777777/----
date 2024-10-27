#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
typedef enum {
	ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, END
} Kind;
typedef enum {
	STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR
} GrammarState;
typedef struct TokenUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct TokenUnit *next;
} Token;
typedef struct ASTUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct ASTUnit *lhs, *mid, *rhs;
} AST;
typedef struct ASMUnit{
    char command[100000][100];
    int id;
}ASM;
/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x) {\
	puts("Compile Error!");\
	if(DEBUG) {\
		fprintf(stderr, "Error at line: %d\n", __LINE__);\
		fprintf(stderr, "Error message: %s\n", x);\
	}\
	exit(0);\
}
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 0
// Split the input char array into token linked list.
Token *lexer(const char *in);
// Create a new token.
Token *new_token(Kind kind, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head);
// Parse the token array. Return the constructed AST.
AST *parser(Token *arr, size_t len);
// Parse the token array. Return the constructed AST.
AST *parse(Token *arr, int l, int r, GrammarState S);
// Create a new AST node.
AST *new_AST(Kind kind, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind));
// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind);
// Return 1 if kind is ADD or SUB.
int condADD(Kind kind);
// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind);
// Return 1 if kind is RPAR.
int condRPAR(Kind kind);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *now);
// Generate ASM code.
void printtree(AST* now);
//check the tree created
int codegen(AST *root,int LR);
// Free the whole AST.
void freeAST(AST *now);

/// debug interfaces

// Print token array.
void token_print(Token *in, size_t len);
// Print AST tree.
void AST_print(AST *head);

char input[MAX_LENGTH];

ASM asms;
int regs=3;
int main() {
    asms.id=0;
	while (fgets(input, MAX_LENGTH, stdin) != NULL) {
        regs=3;
		Token *content = lexer(input);
		size_t len = token_list_to_arr(&content);
		if (len == 0) continue;
		AST *ast_root = parser(content, len);
		semantic_check(ast_root);
		/*printtree(ast_root);*/
		codegen(ast_root,0);
		free(content);
		freeAST(ast_root);
	}
	if(asms.id!=0)
    {
        for(int i=0;i<asms.id;i++)
        {
            puts(asms.command[i]);
        }
    }
	return 0;
}
Token *lexer(const char *in) {
	Token *head = NULL;
	Token **now = &head;
	for (int i = 0; in[i]; i++) {
		if (isspace(in[i])) // ignore space characters
			continue;
		else if (isdigit(in[i])) {
			(*now) = new_token(CONSTANT, atoi(in + i));
			while (in[i+1] && isdigit(in[i+1])) i++;
		}
		else if ('x' <= in[i] && in[i] <= 'z') // variable
			(*now) = new_token(IDENTIFIER, in[i]);
		else switch (in[i]) {
			case '=':
				(*now) = new_token(ASSIGN, 0);
				break;
			case '+':
				if (in[i+1] && in[i+1] == '+') {
					i++;
					// In lexer scope, all "++" will be labeled as PREINC.
					(*now) = new_token(PREINC, 0);
				}
				// In lexer scope, all single "+" will be labeled as PLUS.
				else (*now) = new_token(PLUS, 0);
				break;
			case '-':
				if (in[i+1] && in[i+1] == '-') {
					i++;
					// In lexer scope, all "--" will be labeled as PREDEC.
					(*now) = new_token(PREDEC, 0);
				}
				// In lexer scope, all single "-" will be labeled as MINUS.
				else (*now) = new_token(MINUS, 0);
				break;
			case '*':
				(*now) = new_token(MUL, 0);
				break;
			case '/':
				(*now) = new_token(DIV, 0);
				break;
			case '%':
				(*now) = new_token(REM, 0);
				break;
			case '(':
				(*now) = new_token(LPAR, 0);
				break;
			case ')':
				(*now) = new_token(RPAR, 0);
				break;
			case ';':
				(*now) = new_token(END, 0);
				break;
			default:
				err("Unexpected character.");
		}
		now = &((*now)->next);
	}
	return head;
}

Token *new_token(Kind kind, int val) {
	Token *res = (Token*)malloc(sizeof(Token));
	res->kind = kind;
	res->val = val;
	res->next = NULL;
	return res;
}

size_t token_list_to_arr(Token **head) {
	size_t res;
	Token *now = (*head), *del;
	for (res = 0; now != NULL; res++) now = now->next;
	now = (*head);
	if (res != 0) (*head) = (Token*)malloc(sizeof(Token) * res);
	for (int i = 0; i < res; i++) {
		(*head)[i] = (*now);
		del = now;
		now = now->next;
		free(del);
	}
	return res;
}
AST *parser(Token *arr, size_t len) {
	for (int i = 1; i < len; i++) {
		// correctly identify "ADD" and "SUB"
		if (arr[i].kind == PLUS || arr[i].kind == MINUS) {
			switch (arr[i - 1].kind) {
				case PREINC:
				case PREDEC:
				case IDENTIFIER:
				case CONSTANT:
				case RPAR:
					arr[i].kind = arr[i].kind - PLUS + ADD;
				default: break;
			}
		}
	}
	return parse(arr, 0, len - 1, STMT);
}

AST *parse(Token *arr, int l, int r, GrammarState S) {
	AST *now = NULL;
	if (l > r)
		err("Unexpected parsing range.");
	int nxt;
	switch (S) {
		case STMT:
			if (l == r && arr[l].kind == END)
				return NULL;
			else if (arr[r].kind == END)
				return parse(arr, l, r - 1, EXPR);
			else err("Expected \';\' at the end of line.");
		case EXPR:
			return parse(arr, l, r, ASSIGN_EXPR);
		case ASSIGN_EXPR:
			if ((nxt = findNextSection(arr, l, r, condASSIGN)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, UNARY_EXPR);
				now->rhs = parse(arr, nxt + 1, r, ASSIGN_EXPR);
				return now;
			}
			return parse(arr, l, r, ADD_EXPR);
		case ADD_EXPR:
			if((nxt = findNextSection(arr, r, l, condADD)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, ADD_EXPR);
				now->rhs = parse(arr, nxt + 1, r, MUL_EXPR);
				return now;
			}
			return parse(arr, l, r, MUL_EXPR);
		case MUL_EXPR:
		    if((nxt = findNextSection(arr, r, l, condMUL)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, MUL_EXPR);
				now->rhs = parse(arr, nxt + 1, r, UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, UNARY_EXPR);
			// TODO: Implement MUL_EXPR.
			// hint: Take ADD_EXPR as reference.
		case UNARY_EXPR:
		    if (arr[l].kind == PREINC || arr[l].kind == PREDEC || arr[l].kind==PLUS || arr[l].kind==MINUS) {
				// translate "POSTINC", "POSTDEC" into "PREINC", "PREDEC"
				now = new_AST(arr[l].kind, 0);
				now->mid = parse(arr, l + 1, r , UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, POSTFIX_EXPR);
			// TODO: Implement UNARY_EXPR.
			// hint: Take POSTFIX_EXPR as reference.
		case POSTFIX_EXPR:
			if (arr[r].kind == PREINC || arr[r].kind == PREDEC) {
				// translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
				now = new_AST(arr[r].kind - PREINC + POSTINC, 0);
				now->mid = parse(arr, l, r - 1, POSTFIX_EXPR);
				return now;
			}
			return parse(arr, l, r, PRI_EXPR);
		case PRI_EXPR:
			if (findNextSection(arr, l, r, condRPAR) == r) {
				now = new_AST(LPAR, 0);
				now->mid = parse(arr, l + 1, r - 1, EXPR);
				return now;
			}
			if (l == r) {
				if (arr[l].kind == IDENTIFIER || arr[l].kind == CONSTANT)
					return new_AST(arr[l].kind, arr[l].val);
				err("Unexpected token during parsing.");
			}
			err("No token left for parsing.");
		default:
			err("Unexpected grammar state.");
	}
}

AST *new_AST(Kind kind, int val) {
	AST *res = (AST*)malloc(sizeof(AST));
	res->kind = kind;
	res->val = val;
	res->lhs = res->mid = res->rhs = NULL;
	return res;
}

int findNextSection(Token *arr, int start, int end, int (*cond)(Kind)) {
	int par = 0;
	int d = (start < end) ? 1 : -1;
	for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d) {
		if (arr[i].kind == LPAR) par++;
		if (arr[i].kind == RPAR) par--;
		if (par == 0 && cond(arr[i].kind) == 1) return i;
	}
	return -1;
}

int condASSIGN(Kind kind) {
	return kind == ASSIGN;
}

int condADD(Kind kind) {
	return kind == ADD || kind == SUB;
}

int condMUL(Kind kind) {
	return kind == MUL || kind == DIV || kind == REM;
}

int condRPAR(Kind kind) {
	return kind == RPAR;
}
void printtree(AST* now)
{
    if (now == NULL) return;
	// Left operand of '=' must be an identifier or identifier with one or more parentheses.
	if (now->kind == ASSIGN) {
		printf("ASSIGN\n");
		printtree(now->lhs);
		printtree(now->rhs);
	}
	else if(now->kind==PREINC||now->kind==PREDEC)
    {
        printf("PRE\n");
        printtree(now->mid);
    }
    else if(now->kind==POSTINC||now->kind==POSTDEC)
    {
        printf("POST\n");
        printtree(now->mid);
    }
    else if(now->kind==PLUS||now->kind==MINUS||now->kind==LPAR||now->kind==RPAR)
    {
       printf("+-()\n");
       printtree(now->mid);
    }
    else if(now->kind==ADD||now->kind==SUB||now->kind==MUL||now->kind==DIV||now->kind==REM)
    {
        printf("+-*/\n");
        printtree(now->lhs);
        printtree(now->rhs);
    }
    else
    {
        printf("x5\n");
        return;
    }
}
void semantic_check(AST *now) {
	if (now == NULL) return;
	// Left operand of '=' must be an identifier or identifier with one or more parentheses.
	if (now->kind == ASSIGN) {
		AST *tmp = now->lhs;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");
        semantic_check(now->rhs);
	}
	else if(now->kind==PREINC||now->kind==PREDEC)
    {
        AST *tmp = now->mid;
        while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Rvalue is required as left operand of assignment.");
    }
    else if(now->kind==POSTINC||now->kind==POSTDEC)
    {
        AST *tmp = now->mid;
        while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");

    }
    else if(now->kind==PLUS||now->kind==MINUS||now->kind==LPAR||now->kind==RPAR)
    {
       semantic_check(now->mid);
    }
    else if(now->kind==ADD||now->kind==SUB||now->kind==MUL||now->kind==DIV||now->kind==REM)
    {
        semantic_check(now->lhs);
        semantic_check(now->rhs);
    }
    else
    {
        return;
    }
	// Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
	// TODO: Implement the remaining semantic_check code.
	// hint: Follow the instruction above and ASSIGN-part code to implement.
	// hint: Semantic of each node needs to be checked recursively (from the current node to lhs/mid/rhs node).
}
int reg,isreg=0;
int codegen(AST* now,int LR) 
{
    if(now==NULL)return;
    else if(LR==0)
    {
        if(now->kind==ASSIGN)
        {
            reg=codegen(now->rhs,0);
            codegen(now->lhs,1);
        }
        else if(now->kind==ADD||now->kind==SUB||now->kind==MUL||now->kind==DIV||now->kind==REM)
        {
            if(now->kind==ADD)
            {
                isreg=0;
                int lreg=0,rreg=0;
                int l=codegen(now->lhs,0);
                if(isreg==1)lreg=1;
                isreg=0;
                int r=codegen(now->rhs,0);
                if(isreg==1)rreg=1;
                if(lreg==1&&rreg==1){
                    isreg=1;
                    if(l<3&&r>=3)
                    {
                        sprintf(asms.command[asms.id],"add r%d r%d r%d",r,r,l);
                        asms.id++;
                        return r;
                    }
                    else if(l>=3&&r<3)
                    {
                        sprintf(asms.command[asms.id],"add r%d r%d r%d",l,l,r);
                        asms.id++;
                        return l;
                    }
                    else if(l<3&&r<3)
                    {
                        sprintf(asms.command[asms.id],"add r%d r%d r%d",regs,r,l);
                        int rs=regs;
                        regs++;
                        asms.id++;
                        return rs;
                    }
                    else
                    {
                        sprintf(asms.command[asms.id],"add r%d r%d r%d",l,l,r);
                        regs=r;
                        asms.id++;
                        return l;
                    }
                }
                else if(lreg==1&&rreg==0)
                {
                    isreg=1;
                    if(r>0)
                    {
                        if(l<3)
                        {
                            sprintf(asms.command[asms.id],"add r%d r%d %d",regs,l,r);
                            int rs=regs;
                            regs++;
                            asms.id++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"add r%d r%d %d",l,l,r);
                            asms.id++;
                            return l;
                        }
                    }
                    else if(r<0)
                    {
                        if(l<3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d r%d %d",regs,l,abs(r));
                            int rs=regs;
                            asms.id++;
                            regs++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d r%d %d",l,l,abs(r));
                            asms.id++;
                            return l;
                        }
                    }
                    else
                    {
                        return l;
                    }
                }
                else if(lreg==0&&rreg==1)
                {
                    isreg=1;
                    if(l>0)
                    {
                        if(r<3)
                        {
                            sprintf(asms.command[asms.id],"add r%d r%d %d",regs,r,l);
                            int rs=regs;
                            regs++;
                            asms.id++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"add r%d r%d %d",r,r,l);
                            asms.id++;
                            return r;
                        }
                    }
                    else if(l<0)
                    {
                        if(r<3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"add r%d r%d r%d",regs,r,regs+1);
                            int rs=regs;
                            regs++;
                            asms.id++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"add r%d r%d r%d",r,r,regs);
                            asms.id++;
                            return r;
                        }
                    }
                    else
                    {
                       return r;
                    }
                }
                else
                {
                    return (l+r);
                }
            }
            else if(now->kind==SUB)
            {
                isreg=0;
                int lreg=0,rreg=0;
                int l=codegen(now->lhs,0);
                if(isreg==1)lreg=1;
                isreg=0;
                int r=codegen(now->rhs,0);
                if(isreg==1)rreg=1;
                if(lreg==1&&rreg==1)
                {
                    isreg=1;
                    if(l<3&&r>=3)
                    {
                        sprintf(asms.command[asms.id],"sub r%d r%d r%d",r,l,r);
                        asms.id++;
                        return r;
                    }
                    else if(l>=3&&r<3)
                    {
                        sprintf(asms.command[asms.id],"sub r%d r%d r%d",l,l,r);
                        asms.id++;
                        return l;
                    }
                    else if(l<3&&r<3)
                    {
                        sprintf(asms.command[asms.id],"sub r%d r%d r%d",regs,l,r);
                        int rs=regs;
                        regs++;
                        asms.id++;
                        return rs;
                    }
                    else
                    {
                        sprintf(asms.command[asms.id],"sub r%d r%d r%d",l,l,r);
                        regs=r;
                        asms.id++;
                        return l;
                    }
                }
                else if(lreg==1&&rreg==0)
                {
                    isreg=1;
                    if(r>0)
                    {
                        if(l<3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d r%d %d",regs,l,r);
                            int rs=regs;
                            asms.id++;
                            regs++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d r%d %d",l,l,r);
                            asms.id++;
                            return l;
                        }
                    }
                    else if(r<0)
                    {
                        if(l<3)
                        {
                            sprintf(asms.command[asms.id],"add r%d r%d %d",regs,l,abs(r));
                            int rs=regs;
                            regs++;
                            asms.id++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"add r%d r%d %d",l,l,abs(r));
                            asms.id++;
                            return l;
                        }
                    }
                    else
                    {
                        return l;
                    }
                }
                else if(lreg==0&&rreg==1)
                {
                    isreg=1;
                    if(l>0)
                    {
                        if(r<3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d %d r%d",regs,l,r);
                            int rs=reg;
                            asms.id++;
                            regs++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d %d r%d",r,l,r);
                            asms.id++;
                            return r;
                        }
                    }
                    else if(l<0)
                    {
                        if(r<3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"sub r%d r%d r%d",regs,regs+1,r);
                            int rs=regs;
                            regs++;
                            asms.id++;
                            return rs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"sub r%d r%d r%d",r,regs,r);
                            asms.id++;
                            return r;
                        }
                    }
                    else
                    {
                       if(r>=3)
                       {
                           sprintf(asms.command[asms.id],"sub r%d 0 1",regs);
                           asms.id++;
                           sprintf(asms.command[asms.id],"mul r%d r%d r%d",r,r,regs);
                           asms.id++;
                           return r;
                       }
                       else
                       {
                           sprintf(asms.command[asms.id],"sub r%d 0 1",regs+1);
                           asms.id++;
                           sprintf(asms.command[asms.id],"mul r%d r%d r%d",regs,r,regs+1);
                           asms.id++;
                           int rs=regs;
                           regs++;
                           return rs;
                       }
                    }
               }
               else
               {
                   return (l-r);
               }
            }
            else if(now->kind==MUL)
            {
                isreg=0;
                int lreg=0,rreg=0;
                int l=codegen(now->lhs,0);
                if(isreg==1)lreg=1;
                isreg=0;
                int r=codegen(now->rhs,0);
                if(isreg==1)rreg=1;
                if(lreg==1&&rreg==1)
                {
                    isreg=1;
                    if(l<3&&r<3)
                    {
                       sprintf(asms.command[asms.id],"mul r%d r%d r%d",regs,l,r);
                       int rs=regs;
                       regs++;
                       asms.id++;
                       return rs;
                    }
                    else if(l<3&&r>=3)
                    {
                        sprintf(asms.command[asms.id],"mul r%d r%d r%d",r,r,l);
                        asms.id++;
                        return r;
                    }
                    else if(l>=3&&r<3)
                    {
                        sprintf(asms.command[asms.id],"mul r%d r%d r%d",l,l,r);
                        asms.id++;
                        return l;
                    }
                    else
                    {
                        sprintf(asms.command[asms.id],"mul r%d r%d r%d",l,l,r);
                        regs=r;
                        asms.id++;
                        return l;
                    }
                }
                else if(lreg==1&&rreg==0)
                {
                    if(r>0)
                    {
                       isreg=1;
                       if(l>=3)
                       {
                         sprintf(asms.command[asms.id],"mul r%d r%d %d",l,l,r);
                         asms.id++;
                         return l;
                       }
                       else
                       {
                           sprintf(asms.command[asms.id],"mul r%d r%d %d",regs,l,r);
                           int rs=regs;
                           regs++;
                           asms.id++;
                           return rs;
                       }
                    }
                    else if(r<0)
                    {
                        isreg=1;
                        if(l>=3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(r));
                            asms.id++;
                            sprintf(asms.command[asms.id],"mul r%d r%d r%d",l,l,regs);
                            asms.id++;
                            return l;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(r));
                            asms.id++;
                            sprintf(asms.command[asms.id],"mul r%d r%d r%d",regs,l,regs+1);
                            asms.id++;
                            int rs=regs;
                            regs++;
                            return rs;
                        }

                    }
                    else
                    {
                       isreg=0;
                       return 0;
                    }
                }
                else if(lreg==0&&rreg==1)
                {
                    if(l>0)
                    {
                       isreg=1;
                       if(r>=3)
                       {
                         sprintf(asms.command[asms.id],"mul r%d r%d %d",r,r,l);
                         asms.id++;
                         return r;
                       }
                       else
                       {
                           sprintf(asms.command[asms.id],"mul r%d r%d %d",regs,r,l);
                           int rs=regs;
                           regs++;
                           asms.id++;
                           return rs;
                       }
                    }
                    else if(l<0)
                    {
                        isreg=1;
                        if(r>=3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"mul r%d r%d r%d",r,regs,r);
                            asms.id++;
                            return r;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"mul r%d r%d r%d",regs,regs+1,r);
                            asms.id++;
                            int rs=regs;
                            regs++;
                            return rs;
                        }
                    }
                    else
                    {
                      isreg=0;
                      return 0;
                    }
                }
                else
                {
                    isreg=0;
                    return (l*r);
                }
            }
            else if(now->kind==REM)
            {
                isreg=0;
                int lreg=0,rreg=0;
                int l=codegen(now->lhs,0);
                if(isreg==1)lreg=1;
                isreg=0;
                int r=codegen(now->rhs,0);
                if(isreg==1)rreg=1;
                if(lreg==1&&rreg==1)
                {
                    if(l==r)
                    {
                      isreg=0;
                      return 0;
                    }
                    else
                    {
                        isreg=1;
                        if(l<3&&r>=3)
                        {
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",r,l,r);
                            asms.id++;
                            return r;
                        }
                        else if(l>=3&&r<3)
                        {
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",l,l,r);
                            asms.id++;
                            return l;
                        }
                        else if(l<3&&r<3)
                        {
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",regs,l,r);
                            int rs=regs;
                            regs++;
                            asms.id++;
                            return regs;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",l,l,r);
                            regs=r;
                            asms.id++;
                            return l;
                        }
                    }
                }
                else if(lreg==1&&rreg==0)
                {
                    isreg=1;
                    if(r>0)
                    {
                       if(l>=3)
                       {
                          sprintf(asms.command[asms.id],"rem r%d r%d %d",l,l,r);
                          asms.id++;
                          return l;
                       }
                       else
                       {
                          sprintf(asms.command[asms.id],"rem r%d r%d %d",regs,l,r);
                          int rs=regs;
                          regs++;
                          asms.id++;
                          return rs;
                       }
                    }
                    else if(r==0)
                    {
                        err("mamamia error");
                    }
                    else
                    {
                        if(l>=3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(r));
                            asms.id++;
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",l,l,regs);
                            asms.id++;
                            return l;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(r));
                            asms.id++;
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",regs,l,regs+1);
                            asms.id++;
                            int rs=regs;
                            regs++;
                            return rs;
                        }
                    }
                }
                else if(lreg==0&&rreg==1)
                {
                    isreg=1;
                    if(l>=0)
                    {
                        if(r>=3)
                        {
                           sprintf(asms.command[asms.id],"rem r%d %d r%d",r,l,r);
                           asms.id++;
                           return r;
                        }
                        else
                        {
                          sprintf(asms.command[asms.id],"rem r%d %d r%d",regs,l,r);
                          int rs=regs;
                          regs++;
                          asms.id++;
                          return rs;
                        }
                    }
                    else
                    {
                        isreg=1;
                        if(r>=3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",r,regs,r);
                            asms.id++;
                            return r;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"rem r%d r%d r%d",regs,regs+1,r);
                            asms.id++;
                            int rs=regs;
                            regs++;
                            return rs;
                        }
                    }
                }
                else
                {
                    isreg=0;
                    return (l%r);
                }
            }
            else
            {
                isreg=0;
                int lreg=0,rreg=0;
                int l=codegen(now->lhs,0);
                if(isreg==1)lreg=1;
                isreg=0;
                int r=codegen(now->rhs,0);
                if(isreg==1)rreg=1;
                if(lreg==1&&rreg==1)
                {
                    if(l==r)
                    {
                        isreg=0;
                        return 1;
                    }
                    isreg=1;
                    if(r>=3&&l<3)
                    {
                        sprintf(asms.command[asms.id],"div r%d r%d r%d",r,l,r);
                        asms.id++;
                        return r;
                    }
                    else if(r<3&&l<3)
                    {
                       sprintf(asms.command[asms.id],"div r%d r%d r%d",regs,l,r);
                       int rs=regs;
                       regs++;
                       asms.id++;
                       return rs;
                    }
                    else if(r<3&&l>=3)
                    {
                        sprintf(asms.command[asms.id],"div r%d r%d r%d",l,l,r);
                        asms.id++;
                        return l;
                    }
                    else
                    {
                        sprintf(asms.command[asms.id],"div r%d r%d r%d",l,l,r);
                        regs=r;
                        asms.id++;
                        return l;
                    }
                }
                else if(lreg==1&&rreg==0)
                {
                    isreg=1;
                    if(r>0)
                    {
                       if(l>=3)
                       {
                         sprintf(asms.command[asms.id],"div r%d r%d %d",l,l,r);
                         asms.id++;
                         return l;
                       }
                       else
                       {
                         sprintf(asms.command[asms.id],"div r%d r%d %d",regs,l,r);
                         int rs=regs;
                         regs++;
                         asms.id++;
                         return rs;
                       }
                    }
                    else if(r==0)
                    {
                        err("WTF div zero?");
                    }
                    else
                    {
                        if(l>=3)
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(r));
                            asms.id++;
                            sprintf(asms.command[asms.id],"div r%d r%d r%d",l,l,regs);
                            asms.id++;
                            return l;
                        }
                        else
                        {
                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(r));
                            asms.id++;
                            sprintf(asms.command[asms.id],"div r%d r%d r%d",regs,l,regs+1);
                            asms.id++;
                            int rs=regs;
                            regs++;
                            return rs;
                        }
                    }
                }
                else if(lreg==0&&rreg==1)
                {
                    isreg=1;
                    if(l>0)
                    {
                        if(r>=3)
                        {
                          sprintf(asms.command[asms.id],"div r%d %d r%d",r,l,r);
                          asms.id++;
                          return r;
                        }
                        else
                        {
                          sprintf(asms.command[asms.id],"div r%d %d r%d",regs,l,r);
                          int rs=regs;
                          regs++;
                          asms.id++;
                          return rs;
                        }
                    }
                    else if(l==0)
                    {
                        isreg=0;
                        return 0;
                    }
                    else
                    {
                        if(r>=3)
                        {

                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"div r%d r%d r%d",r,regs,r);
                            asms.id++;
                            return r;
                        }
                        else
                        {

                            sprintf(asms.command[asms.id],"sub r%d 0 %d",regs+1,abs(l));
                            asms.id++;
                            sprintf(asms.command[asms.id],"div r%d r%d r%d",regs,regs+1,r);
                            asms.id++;
                            int rs=regs;
                            regs++;
                            return rs;
                        }
                    }
                }
                else
                {
                    isreg=0;
                    return (l/r);
                }
            }
        }
        else if(now->kind==PREINC||now->kind==PREDEC)
        {
            int val=codegen(now->mid,0);
            if(now->kind==PREINC)
            {
                sprintf(asms.command[asms.id],"add r%d r%d 1",regs,val);
                int rs=regs;
                regs++;
                asms.id++;
                if(val==0)
                {
                    sprintf(asms.command[asms.id],"store [0] r%d",rs);
                    asms.id++;
                }
                else if(val==1)
                {
                    sprintf(asms.command[asms.id],"store [4] r%d",rs);
                    asms.id++;
                }
                else if(val==2)
                {
                    sprintf(asms.command[asms.id],"store [8] r%d",rs);
                    asms.id++;
                }
                return rs;
            }
            else
            {
                sprintf(asms.command[asms.id],"sub r%d r%d 1",regs,val);
                int rs=regs;
                regs++;
                asms.id++;
                if(val==0)
                {
                    sprintf(asms.command[asms.id],"store [0] r%d",rs);
                    asms.id++;
                }
                else if(val==1)
                {
                    sprintf(asms.command[asms.id],"store [4] r%d",rs);
                    asms.id++;
                }
                else if(val==2)
                {
                    sprintf(asms.command[asms.id],"store [8] r%d",rs);
                    asms.id++;
                }
                return rs;
            }
        }
        else if(now->kind==POSTDEC||now->kind==POSTINC)
        {
            int val=codegen(now->mid,0);
            if(now->kind==POSTINC)
            {
                sprintf(asms.command[asms.id],"add r%d r%d 1",regs,val);
            }
            else
            {
               sprintf(asms.command[asms.id],"sub r%d r%d 1",regs,val);
            }
            asms.id++;
            if(val==0)
            {
               sprintf(asms.command[asms.id],"store [0] r%d",regs);
            }
            else if(val==1)
            {
               sprintf(asms.command[asms.id],"store [4] r%d",regs);
            }
            else if(val==2)
            {
                sprintf(asms.command[asms.id],"store [8] r%d",regs);
            }
            asms.id++;
            return val;
        }
        else if(now->kind==LPAR||now->kind==RPAR)
        {
            return codegen(now->mid,0);
        }
        else if(now->kind==PLUS||now->kind==MINUS)
        {
            if(now->kind==PLUS)
            {
                return codegen(now->mid,0);
            }
            else
            {
                int val=codegen(now->mid,0);
                if(isreg==0)
                {
                    return -1*val;
                }
                else
                {
                    isreg=1;
                    sprintf(asms.command[asms.id],"sub r%d 0 1",regs);
                    asms.id++;
                    if(val>=3)
                    {
                        sprintf(asms.command[asms.id],"mul r%d r%d r%d",val,val,regs);
                        asms.id++;
                        return val;
                    }
                    else
                    {
                        sprintf(asms.command[asms.id],"mul r%d r%d r%d",regs,val,regs);
                        int rs=regs;
                        regs++;
                        asms.id++;
                        return rs;
                    }
                }
            }
        }
        else if(now->kind==IDENTIFIER)
        {
            if(now->val==120)
            {
                sprintf(asms.command[asms.id],"load r%d [%d]",0,0);
                asms.id++;
                isreg=1;
                return 0;
            }
            else if(now->val==121)
            {
                sprintf(asms.command[asms.id],"load r%d [%d]",1,4);
                isreg=1;
                asms.id++;
                return 1;
            }
            else if(now->val==122)
            {
                sprintf(asms.command[asms.id],"load r%d [%d]",2,8);
                isreg=1;
                asms.id++;
                return 2;
            }
        }
        else if(now->kind==CONSTANT)
        {
            isreg=0;
            return now->val;
        }
        else if(now->kind==END)
        {
            printf("endhere\n");
        }
    }
    else if(LR==1)
    {
        while (now->kind == LPAR) now = now->mid;
        if(isreg==1)
        {
            if(now->val==120)
            {
                sprintf(asms.command[asms.id],"store [%d] r%d",0,reg);
            }
            else if(now->val==121)
            {
                sprintf(asms.command[asms.id],"store [%d] r%d",4,reg);
            }
            else if(now->val==122)
            {
                sprintf(asms.command[asms.id],"store [%d] r%d",8,reg);
            }
            asms.id++;
        }
        else
        {
            if(reg>=0)
            {
              sprintf(asms.command[asms.id],"add r3 0 %d",reg);
            }
            else
            {
              sprintf(asms.command[asms.id],"sub r3 0 %d",abs(reg));
            }
            asms.id++;
            if(now->val==120)
            {
                sprintf(asms.command[asms.id],"store [%d] r3",0);
            }
            else if(now->val==121)
            {
                sprintf(asms.command[asms.id],"store [%d] r3",4);
            }
            else if(now->val==122)
            {
                sprintf(asms.command[asms.id],"store [%d] r3",8);
            }
            asms.id++;
        }
        return reg;
    }
	// TODO: Implement your codegen in your own way.
	// You may modify the function parameter or the return type, even the whole structure as you wish.
}

void freeAST(AST *now) {
	if (now == NULL) return;
	freeAST(now->lhs);
	freeAST(now->mid);
	freeAST(now->rhs);
	free(now);
}

void token_print(Token *in, size_t len) {
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus", "End"
	};
	const static char KindSymbol[][20] = {
		"'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"
	};
	const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
	const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
	for(int i = 0; i < len; i++) {
		switch(in[i].kind) {
			case LPAR:
			case RPAR:
			case PREINC:
			case PREDEC:
			case ADD:
			case SUB:
			case MUL:
			case DIV:
			case REM:
			case ASSIGN:
			case PLUS:
			case MINUS:
				printf(format_str, i, KindName[in[i].kind], "symbol", KindSymbol[in[i].kind]);
				break;
			case CONSTANT:
				printf(format_int, i, KindName[in[i].kind], "value", in[i].val);
				break;
			case IDENTIFIER:
				printf(format_str, i, KindName[in[i].kind], "name", (char*)(&(in[i].val)));
				break;
			case END:
				printf("<Index = %3d>: %-10s\n", i, KindName[in[i].kind]);
				break;
			default:
				puts("=== unknown token ===");
		}
	}
}

void AST_print(AST *head) {
	static char indent_str[MAX_LENGTH] = "  ";
	static int indent = 2;
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"
	};
	const static char format[] = "%s\n";
	const static char format_str[] = "%s, <%s = %s>\n";
	const static char format_val[] = "%s, <%s = %d>\n";
	if (head == NULL) return;
	char *indent_now = indent_str + indent;
	indent_str[indent - 1] = '-';
	printf("%s", indent_str);
	indent_str[indent - 1] = ' ';
	if (indent_str[indent - 2] == '`')
		indent_str[indent - 2] = ' ';
	switch (head->kind) {
		case ASSIGN:
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
		case PREINC:
		case PREDEC:
		case POSTINC:
		case POSTDEC:
		case LPAR:
		case RPAR:
		case PLUS:
		case MINUS:
			printf(format, KindName[head->kind]);
			break;
		case IDENTIFIER:
			printf(format_str, KindName[head->kind], "name", (char*)&(head->val));
			break;
		case CONSTANT:
			printf(format_val, KindName[head->kind], "value", head->val);
			break;
		default:
			puts("=== unknown AST type ===");
	}
	indent += 2;
	strcpy(indent_now, "| ");
	AST_print(head->lhs);
	strcpy(indent_now, "` ");
	AST_print(head->mid);
	AST_print(head->rhs);
	indent -= 2;
	(*indent_now) = '\0';
}
