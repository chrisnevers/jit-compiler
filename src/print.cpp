#include <iostream>
#include "ast.h"

using namespace std;

void display_m (size_t tag, M* m, bool nl) {
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
        case TMTru: { cout << "true"; break; }
        case TMFals: { cout << "false"; break; }
        case TMPair: {
            MPair* v = (MPair*) m;
            cout << "(";
            display_m (v->l->tag, v->l, false);
            cout << ", ";
            display_m (v->r->tag, v->r, false);
            cout << ")";
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
        case TMIf: {
            MIf* i = (MIf*) m;
            cout << "if ";
            display_m (i->cnd->tag, i->cnd, false);
            cout << " then ";
            display_m (i->thn->tag, i->thn, false);
            cout << " else ";
            display_m (i->els->tag, i->els, false);
            break;
        }
        case TMPrm: {
            MPrm* pr = (MPrm*) m;
            switch (pr->op) {
                case TPLt: cout << "<"; break;
                case TPLtE: cout << "<="; break;
                case TPGt: cout << ">"; break;
                case TPGtE: cout << ">="; break;
                case TPEq: cout << "="; break;
                case TPFst: cout << "fst"; break;
                case TPSnd: cout << "snd"; break;
                case TPAdd: cout << "+"; break;
                case TPNeg:
                case TPSub: cout << "-"; break;
                case TPMkPair: cout << "mkpair"; break;
                case TPRead: cout << "read"; break;
                default: break;
            };
            cout << "(";
            for (size_t i = 0; i < pr->arity; ++i) {
                display_m (pr->ms[i]->tag, pr->ms[i], false);
                if (i + 1 != pr->arity) {
                    cout << ", ";
                }
            }
            cout << ")";
            break;
        }
        default: {
            cout << "Unknown M: " << tag << endl;
            break;
        }
    }
    if (nl) {
        cout << endl;
    }
}

void display_e (size_t tag, E* e) {
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

void display_k (size_t tag, K* k) {
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
        case TKIf: {
            KIf* i = (KIf*) k;
            cout << "if (";
            display_m (i->t->tag, i->t, false);
            cout << ", ";
            display_m (i->e->tag, i->e, false);
            cout << ", ";
            display_k (i->ok->tag, i->ok);
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
                case TPFst: cout << "fst"; break;
                case TPSnd: cout << "snd"; break;
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
                case TPLt: cout << "<"; break;
                case TPLtE: cout << "<="; break;
                case TPGt: cout << ">"; break;
                case TPGtE: cout << ">="; break;
                case TPEq: cout << "="; break;
                case TPMkPair: cout << "mkpair"; break;
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
            cout << "Unknown K: " << tag << endl;
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
