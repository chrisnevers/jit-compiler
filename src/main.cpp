#include <iostream>
#include <string>
#include <stdexcept>
#include "ast.h"

using namespace std;

const int heap_size = 1 << 14;
size_t heap[heap_size] = {};
int heap_max = heap_size - 1;

size_t pc_i = 0;
int pe = 0;
int pk = 0;
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

void cek () {
    E* pe = e_mt();
    K* pk = k_ret();

    cout << "Starting CEK at " << pc_i;
    size_t pc = heap[pc_i];
    cout << ": " << pc << endl;

    while (true) {
        // cout << "Looping pc: " << pc << endl;
        M* m = (struct M*) pc;
        int tag = m->tag;

        cout << "Tag: " << tag << endl;

        display_m (m->tag, m, false);
        cout << ", ";
        display_e (pe->tag, pe);
        cout << ", ";
        display_k (pk->tag, pk);
        cout << endl;

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
                pe = e_mt ();
                switch (pk->tag) {
                    case TKRet: {
                        if (is_v (tag) && pe->tag == TEMt) {
                            cout << "Result: "; display_m (tag, (M*) pc);
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
                            // display_e (env->tag, env);
                            E* ne = e_clo (lam->id, (M*) pc, env);
                            pc = (size_t) lam->body;
                            pe = ne;
                            pk = ar->ok;
                        } else {
                            throw logic_error ("Expected Closure in KArg");
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
                // cout << "Inserting null at " << i << endl;
                heap[i++]   = (size_t) node;
                break;
            }
            case TMNum: {
                fscanf (fp, "%s", tmp);
                MNum* node  = (MNum*) malloc (sizeof (MNum));
                node->m     = mk_m (TMNum);
                node->val   = stoi (tmp, nullptr, 2);
                // cout << "Inserting Number at " << i << endl;
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
                // cout << i << endl;
                i += 2;
                break;
            }
            case TMApp: {
                MApp* node  = (MApp*) malloc (sizeof (MApp));
                node->m     = mk_m (TMApp);
                fscanf (fp, "%s", tmp);
                int fn_p    = stoi (tmp, nullptr, 2);
                // cout << tmp << " : FN: " << fn_p << endl;
                node->fn    = (M*) heap[fn_p];
                // display_m (node->fn->tag, node->fn);
                fscanf (fp, "%s", tmp);
                int arg_p   = stoi (tmp, nullptr, 2);
                // cout << tmp << " : ARG: " << arg_p << endl;
                node->arg   = (M*) heap[arg_p];
                // display_m (node->arg->tag, node->arg);
                // cout << "App at " << i << endl;
                heap[i++]   = (size_t) node;
                i += 2;
                // display_m (TMApp, (M*)node);
                break;
            }
            default: {
                cout << "Unknown tag: " << tag << endl;
                break;
            }
        }
    }
    // cout << "pf = " << i << endl;
    // pf = i;

    // cout << "PC: " << pc_i << endl;
    // cout << "PF: " << pf << endl;
    for (int i = 0; i <= pf; ++i) {
        cout << "HEAP[" << i << "]" << heap[i] << endl;
    }

    return;
}

int main () {
    read_file ("tmp.byte");
    cek ();
    return 0;
}
