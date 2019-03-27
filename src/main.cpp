#include <iostream>
#include <string>
#include <stdexcept>
#include "ast.h"

using namespace std;

const int heap_size = 1 << 14;
size_t heap[heap_size] = {};
int heap_max = heap_size - 1;

size_t pc_i = 0;
int pf = 0;

char* alloc_bc () {
    return (char*) malloc (sizeof(char) * 4);
}

M mk_m (int tag) {
    return (M) { .tag = tag };
}

E mk_e (int tag) {
    return (E) { .tag = tag };
}

K mk_k (int tag) {
    return (K) { .tag = tag };
}

M* m_nul () {
    MNul* node  = (MNul*) malloc (sizeof (MNul));
    node->m     = mk_m (TMNul);
    return (M*) node;
}

E* e_mt () {
    EMt* node   = (EMt*) malloc (sizeof (EMt));
    node->e     = mk_e (TEMt);
    return (E*) node;
}

E* e_clo (int id, M* val, E* next) {
    EClo* node  = (EClo*) malloc (sizeof (EClo));
    node->e     = mk_e (TEClo);
    node->id    = id;
    node->val   = val;
    node->nxt   = next;
    return (E*) node;
}

K* k_ret () {
    KRet* node  = (KRet*) malloc (sizeof (KRet));
    node->k     = mk_k (TKRet);
    return (K*) node;
}

K* k_fn (M* m, E* e, K* ok) {
    KFn* node   = (KFn*) malloc (sizeof (KFn));
    node->k     = mk_k (TKFn);
    node->m     = m;
    node->e     = e;
    node->ok    = ok;
    return (K*) node;
}

K* k_arg (M* m, K* ok) {
    KArg* node   = (KArg*) malloc (sizeof (KArg));
    node->k     = mk_k (TKArg);
    node->m     = m;
    node->ok    = ok;
    return (K*) node;
}

int alloc (int size) {
    if (pf + size > heap_max) {
        throw logic_error ("Out of memory");
    }
    pf += size;
    return pf - size;
}

bool is_v (int tag) {
    switch (tag) {
        case TMNum: return true;
        case TMClo: return true;
        default: return false;
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

void display_heap () {
    for (int i = 0; i <= pf; ++i) {
        if (heap[i] != 0) {
            M* hi = (M*) heap[i];
            cout << "HEAP[" << i << "]" << heap[i] << ": ";
            display_m (hi->tag, hi);
        } else {
            cout << "HEAP[" << i << "]" << heap[i] << endl;
        }
    }
}

void cek () {
    E* pe = e_mt();
    K* pk = k_ret();

    size_t pc = heap[pc_i];

    while (true) {
        M* m = (struct M*) pc;
        int tag = m->tag;

        display_state ((M*)pc, pe, pk);
        // getchar();

        switch (tag) {
            case TMLam: {
                MClo* clo   = (MClo*) malloc (sizeof (MClo));
                clo->m      = mk_m (TMClo);
                clo->ex     = (M*) pc;
                clo->env    = pe;
                pc          = (size_t) clo;
                pe          = e_mt ();
                break;
            }
            case TMApp: {
                MApp* app   = (MApp*) m;
                pc = (size_t) app->fn;
                pk = k_fn (app->arg, pe, pk);
                break;
            }
            case TMVar: {
                MVar* var = (MVar*) m;
                E* e = pe;
                size_t old_pc = pc;
                while (e->tag != TEMt) {
                    EClo* clo = (EClo*) e;
                    if (var->id == clo->id) {
                        pc = (size_t) clo->val;
                        break;
                    } else {
                        e = clo->nxt;
                    }
                }
                if (pc == old_pc) {
                    throw logic_error
                        ("Unbound variable: v" + to_string (var->id));
                }
                break;
            }
            case TMNul:
            case TMClo:
            case TMNum: {
                switch (pk->tag) {
                    case TKRet: {
                        if (is_v (tag)) {
                            display_m (tag, (M*) pc);
                            return;
                        }
                        break;
                    }
                    case TKFn: {
                        KFn* fn = (KFn*) pk;
                        pk = k_arg ((M*) pc, fn->ok);
                        pc = (size_t) fn->m;
                        pe = fn->e;
                        break;
                    }
                    case TKArg: {
                        KArg* ar = (KArg*) pk;
                        if (ar->m->tag == TMClo) {
                            MClo* clo = (MClo*) ar->m;
                            MLam* lam = (MLam*) clo->ex;
                            E* env = (clo->env->tag == TEMt)
                                ? e_mt () : clo->env;
                            E* ne = e_clo (lam->id, (M*) pc, env);
                            pc = (size_t) lam->body;
                            pe = ne;
                            pk = ar->ok;
                        } else {
                            throw logic_error ("Expected Closure in KArg");
                        }
                        break;
                    }
                    case TKOp1: {
                        KOp1* op = (KOp1*) pk;
                        MNum* val = (MNum*) malloc (sizeof (MNum));
                        val->m = mk_m (TMNum);
                        switch (op->op) {
                            case TPNeg: {
                                MNum* r = (MNum*) pc;
                                val->val = - r->val;
                                break;
                            }
                            default: {
                                throw logic_error ("Unknown operator");
                            }
                        };
                        pc = (size_t) val;
                        pe = e_mt ();
                        pk = op->ok;
                        break;
                    }
                    case TKOp2: {
                        KOp2* op = (KOp2*) pk;
                        // Solve second value now
                        if (op->m->tag != TMNul) {
                            KOp2* nk = (KOp2*) malloc (sizeof (KOp2));
                            M* nul  = m_nul ();
                            nk->k   = op->k;
                            nk->op  = op->op;
                            nk->v   = m;
                            nk->m   = nul;
                            nk->ok  = op->ok;
                            pc = (size_t) op->m;
                            pk = (K*) nk;
                        } else {
                            // Values have been computed
                            MNum* val = (MNum*) malloc (sizeof (MNum));
                            val->m      = mk_m (TMNum);
                            switch (op->op) {
                                case TPAdd: {
                                    MNum* l = (MNum*) op->v;
                                    MNum* r = (MNum*) pc;
                                    val->val = l->val + r->val;
                                    break;
                                }
                                case TPSub: {
                                    MNum* l = (MNum*) op->v;
                                    MNum* r = (MNum*) pc;
                                    val->val = l->val - r->val;
                                    break;
                                }
                                default: {
                                    throw logic_error ("Unknown operator");
                                }
                            };
                            pc = (size_t) val;
                            pe = e_mt ();
                            pk = op->ok;
                        }
                        break;
                    }
                    default: {
                        cout << "Unknown K tag: " << pk->tag << endl;
                        break;
                    }
                }
                break;
            }
            case TMPrm: {
                MPrm* prm = (MPrm*) m;
                switch (prm->arity) {
                    case 0: {
                        switch (prm->op) {
                            case TPRead: {
                                int i = 0;
                                scanf ("%d", &i);
                                MNum* n = (MNum*) malloc (sizeof (MNum));
                                n->m    = mk_m (TMNum);
                                n->val  = i;
                                pc = (size_t) n;
                                break;
                            }
                            default: {
                                throw logic_error ("Unknown primitive op");
                            }
                        }
                        break;
                    }
                    case 1: {
                        KOp1* kop = (KOp1*) malloc (sizeof (KOp1));
                        kop->k  = mk_k (TKOp1);
                        kop->op = prm->op;
                        kop->v  = m_nul();
                        kop->ok = pk;
                        pc = (size_t) prm->ms[0];
                        pk = (K*) kop;
                        break;
                    }
                    case 2: {
                        KOp2* kop = (KOp2*) malloc (sizeof (KOp2));
                        kop->k  = mk_k (TKOp2);
                        kop->op = prm->op;
                        kop->m  = prm->ms[1];
                        kop->ok = pk;
                        kop->v  = m_nul ();
                        pc  = (size_t) prm->ms[0];
                        pk  = (K*) kop;
                        break;
                    }
                    default: {
                        throw logic_error ("Unknown primitive operation arity");
                    }
                }
                break;
            }
            default: {
                cout << "Unknown M: " << tag << endl;
            }
        }
    }
}

void read_file (const char* filename) {
    FILE *fp;
    fp = fopen (filename, "r");

    char* tmp = alloc_bc ();
    fscanf (fp, "%s", tmp);
    pf = stoi (tmp, nullptr, 2);
    fscanf (fp, "%s", tmp);
    pc_i = stoi (tmp, nullptr, 2);

    int i = 0;

    while (fscanf (fp, "%s", tmp) != EOF) {
        int tag = stoi (tmp, nullptr, 2);
        switch (tag) {
            case TMNul: {
                MNul* node  = (MNul*) malloc (sizeof (MNul));
                node->m     = mk_m (TMNul);
                heap[i++]   = (size_t) node;
                break;
            }
            case TMNum: {
                fscanf (fp, "%s", tmp);
                MNum* node  = (MNum*) malloc (sizeof (MNum));
                node->m     = mk_m (TMNum);
                node->val   = stoi (tmp, nullptr, 2);
                heap[i++]   = (size_t) node;
                i++;
                break;
            }
            case TMVar: {
                fscanf (fp, "%s", tmp);
                MVar* node  = (MVar*) malloc (sizeof (MVar));
                node->m     = mk_m (TMVar);
                node->id    = stoi (tmp, nullptr, 2);
                heap[i++]   = (size_t) node;
                i++;
                break;
            }
            case TMLam: {
                fscanf (fp, "%s", tmp);
                MLam* node  = (MLam*) malloc (sizeof (MLam));
                node->m     = mk_m (TMLam);
                node->id    = stoi (tmp, nullptr, 2);
                fscanf (fp, "%s", tmp);
                int ptr     = stoi (tmp, nullptr, 2);
                node->body  = (M*) heap[ptr];
                heap[i++]   = (size_t) node;
                i += 2;
                break;
            }
            case TMApp: {
                MApp* node  = (MApp*) malloc (sizeof (MApp));
                node->m     = mk_m (TMApp);
                fscanf (fp, "%s", tmp);
                int fn_p    = stoi (tmp, nullptr, 2);
                node->fn    = (M*) heap[fn_p];
                fscanf (fp, "%s", tmp);
                int arg_p   = stoi (tmp, nullptr, 2);
                node->arg   = (M*) heap[arg_p];
                heap[i++]   = (size_t) node;
                i += 2;
                break;
            }
            case TMPrm: {
                MPrm* node  = (MPrm*) malloc (sizeof (MPrm));
                node->m     = mk_m (TMPrm);
                fscanf (fp, "%s", tmp);
                node->op    = stoi (tmp, nullptr, 2);
                switch (node->op) {
                    case TPSub:
                    case TPAdd: {
                        fscanf (fp, "%s", tmp);
                        int l_p = stoi (tmp, nullptr, 2);
                        fscanf (fp, "%s", tmp);
                        int r_p = stoi (tmp, nullptr, 2);
                        node->arity = 2;
                        node->ms = (M**) malloc (2 * sizeof (M*));
                        node->ms[0] = (M*) heap[l_p];
                        node->ms[1] = (M*) heap[r_p];
                        heap[i++]   = (size_t) node;
                        i += 3;
                        break;
                    }
                    case TPNeg: {
                        fscanf (fp, "%s", tmp);
                        int m_p = stoi (tmp, nullptr, 2);
                        node->arity = 1;
                        node->ms = (M**) malloc (sizeof (M*));
                        node->ms[0] = (M*) heap[m_p];
                        heap[i++]   = (size_t) node;
                        i += 2;
                        break;
                    }
                    case TPRead: {
                        node->arity = 0;
                        heap[i++] = (size_t) node;
                        i++;
                        break;
                    }
                    default: {
                        throw logic_error ("Unknown primitive operator: "
                            + to_string (node->op));
                    }
                }

                break;
            }
            default: {
                cout << "Unknown tag: " << tag << endl;
                break;
            }
        }
    }

    // display_heap();

    return;
}

int main () {
    read_file ("tmp.byte");
    cek ();
    return 0;
}
