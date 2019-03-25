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
        default: {
            cout << "Unknown E: " << tag;
            break;
        }
    }
}

void display_k (int tag, K* k) {
    switch (tag) {
        case TKRet: {
            cout << "ret" << endl;
            break;
        }
        default: {
            cout << "Unknown K" << endl;
            break;
        }
    }
}
