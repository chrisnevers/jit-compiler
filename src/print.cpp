#include <iostream>
#include "ast.h"

using namespace std;

void display_m (int tag, M* m, bool nl) {
    switch (tag) {
        case TMNul: {
            cout << "⊥";
            break;
        }
        case TMNum: {
            MNum* n = (MNum*) m;
            cout << n->val;
            break;
        }
        case TMVar: {
            MVar* v = (MVar*) m;
            cout << "v" << v->id;
            break;
        }
        case TMLam: {
            MLam* lam = (MLam*) m;
            cout << "λv" << lam->id << ".";
            display_m (lam->body->tag, lam->body, false);
            break;
        }
        case TMClo: {
            MClo* clo = (MClo*) m;
            cout << "Clo (";
            display_m (clo->ex->tag, clo->ex, false);
            cout << ", ";
            display_e (clo->env->tag, clo->env);
            cout << ")";
            break;
        }
        case TMApp: {
            MApp* app = (MApp*) m;
            cout << "(";
            display_m (app->fn->tag, app->fn, false);
            cout << " ";
            display_m (app->arg->tag, app->arg, false);
            cout << ")";
            // cout << "Fn Address: " << app->fn << endl;
            // cout << "Arg Address: " << app->arg << endl;
            break;
        }
        case TMPrm: {
            MPrm* pr = (MPrm*) m;
            switch (pr->op) {
                case TPAdd: cout << "+"; break;
                case TPNeg:
                case TPSub: cout << "-"; break;
                case TPRead: cout << "read"; break;
                default: break;
            };
            cout << "(";
            for (int i = 0; i < pr->arity; ++i) {
                display_m (pr->ms[i]->tag, pr->ms[i], false);
                if (i + 1 != pr->arity) {
                    cout << ", ";
                }
            }
            cout << ")";
            break;
        }
        default: {
            cout << "Unknown M" << endl;
            break;
        }
    }
    if (nl) {
        cout << endl;
    }
}

void display_e (int tag, E* e) {
    switch (tag) {
        case TEMt: {
            cout << "*";
            break;
        }
        case TEClo: {
            EClo* clo = (EClo*) e;
            display_e (clo->nxt->tag, clo->nxt);
            cout << "[v" << clo->id << " → ";
            display_m (clo->val->tag, clo->val, false);
            cout << "]";
            break;
        }
        default: {
            cout << "Unknown E: " << tag;
            break;
        }
    }
}

void display_k (int tag, K* k) {
    switch (tag) {
        case TKRet: {
            cout << "ret";
            break;
        }
        case TKFn: {
            KFn* fn = (KFn*) k;
            cout << "fn (";
            display_m (fn->m->tag, fn->m, false);
            cout << ", ";
            display_e (fn->e->tag, fn->e);
            cout << ", ";
            display_k (fn->ok->tag, fn->ok);
            cout << ")";
            break;
        }
        case TKArg: {
            KArg* ar = (KArg*) k;
            cout << "arg (";
            display_m (ar->m->tag, ar->m, false);
            cout << ", ";
            display_k (ar->ok->tag, ar->ok);
            cout << ")";
            break;
        }
        case TKOp0: {
            KOp1* op = (KOp1*) k;
            cout << "op0 (";
            switch (op->op) {
                case TPRead: cout << "read"; break;
                default: break;
            };
            cout << ")";
            break;
        }
        case TKOp1: {
            KOp1* op = (KOp1*) k;
            cout << "op1 (";
            switch (op->op) {
                case TPNeg: cout << "-"; break;
                default: break;
            };
            cout << ", ";
            display_m (op->v->tag, op->v, false);
            cout << ", ";
            display_k (op->ok->tag, op->ok);
            cout << ")";
            break;
        }
        case TKOp2: {
            KOp2* op = (KOp2*) k;
            cout << "op2 (";
            switch (op->op) {
                case TPAdd: cout << "+"; break;
                case TPSub: cout << "-"; break;
                default: break;
            };
            cout << ", ";
            display_m (op->v->tag, op->v, false);
            cout << ", ";
            display_m (op->m->tag, op->m, false);
            cout << ", ";
            display_k (op->ok->tag, op->ok);
            cout << ")";
            break;
        }
        default: {
            cout << "Unknown K" << endl;
            break;
        }
    }
}

void display_state (M* m, E* pe, K* pk) {
    display_m (m->tag, m, false);
    cout << ", ";
    display_e (pe->tag, pe);
    cout << ", ";
    display_k (pk->tag, pk);
    cout << endl;
}

void display_heap (int pf, char* mm) {
    cout << "PF: " << pf << endl;
    for (int i = 0; i < pf * 8 + 16; i+= 8) {
        printf ("%d (%d): %.8s\n", i / 8, i, mm + i);
    }
}
