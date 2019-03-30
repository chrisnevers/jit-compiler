#include <iostream>
#include <string>
#include <stdexcept>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "ast.h"

using namespace std;

bool debug = true;

// Temp string for reading in bytecode
char* tmp = (char*) malloc (8 * sizeof (char));
char* mm;       // Mapped memory location
int fd, fs;     // File descriptor and file size of byte code file

size_t *heap;
size_t pc = 0;
size_t pf = 0;

M mk_m (int tag) {
    return (M) { .tag = tag };
}

E mk_e (int tag) {
    return (E) { .tag = tag };
}

K mk_k (int tag) {
    return (K) { .tag = tag };
}

void* malloc1 (int size) {
    return malloc (size);
}

M* m_nul () {
    MNul* node  = (MNul*) malloc1 (sizeof (MNul));
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
    cout << "PF: " << pf << endl;
    for (int i = 0; i < pf * 8 + 16; i+= 8) {
        printf ("%d (%d): %.8s\n", i / 8, i, mm + i);
    }
}

void cek () {
    E* pe = e_mt();
    K* pk = k_ret();

    while (true) {
        M* m = (struct M*) pc;
        int tag = m->tag;

        display_state ((M*)pc, pe, pk);

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
                MApp* app = (MApp*) m;
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

int get_file_size (const char* filename) {
    struct stat buf;
    stat (filename, &buf);
    return buf.st_size;
}

int get_int (char* tmp, int pos, int off) {
    sprintf (tmp, "%.8s", mm + (pos + off + 2) * 8);
    return stoi (tmp, nullptr, 2);
}

M* load_obj (int pos) {
    int op = get_int (tmp, pos, 0);
    switch (op) {
        case TMNul: return m_nul ();
        case TMNum: {
            MNum* n = (MNum*) malloc1 (sizeof (MNum));
            n->m    = mk_m (TMNum);
            n->val  = get_int (tmp, pos, 1);
            return (M*) n;
        }
        case TMVar: {
            MVar* n = (MVar*) malloc1 (sizeof (MVar));
            n->m    = mk_m (TMVar);
            n->id   = get_int (tmp, pos, 1);
            return (M*) n;
        }
        case TMApp: {
            MApp* n = (MApp*) malloc1 (sizeof (MApp));
            n->m    = mk_m (TMApp);
            n->fn   = load_obj (get_int (tmp, pos, 1));
            n->arg  = load_obj (get_int (tmp, pos, 2));
            return (M*) n;
        }
        case TMLam: {
            MLam* n = (MLam*) malloc1 (sizeof (MLam));
            n->m    = mk_m (TMLam);
            n->id   = get_int (tmp, pos, 1);
            n->body = load_obj (get_int (tmp, pos, 2));
            return (M*) n;
        }
        case TMPrm: {
            MPrm* n = (MPrm*) malloc1 (sizeof (MPrm));
            n->m    = mk_m (TMPrm);
            n->op   = get_int (tmp, pos, 1);
            switch (n->op) {
                case TPSub:
                case TPAdd: {
                    n->arity = 2;
                    n->ms    = (M**) malloc1 (2 * sizeof (M*));
                    n->ms[0] = load_obj (get_int (tmp, pos, 2));
                    n->ms[1] = load_obj (get_int (tmp, pos, 3));
                    break;
                }
                case TPNeg: {
                    n->arity = 1;
                    n->ms    = (M**) malloc1 (sizeof (M*));
                    n->ms[0] = load_obj (get_int (tmp, pos, 2));
                    break;
                }
                case TPRead: {
                    n->arity = 0;
                    break;
                }
                default: throw logic_error ("Unknown primitive operator");
            }
            return (M*) n;
        }
        default: {
            throw logic_error ("Unknown M Tag in byte code");
        }
    };
}

void read_file (const char* filename) {

    // Map file into memory
    fd = open (filename, 0);
    fs = get_file_size (filename);
    mm = (char*) mmap (NULL, fs, PROT_READ, MAP_PRIVATE, fd, 0);

    // Read in length of byte code (pf)
    // and starting instruction (pc)
    pf      = get_int (tmp, -2, 0);;
    int pos = get_int (tmp, -1, 0);

    // Load structures for program
    pc = (size_t) load_obj (pos);

    return;
}

void clean_up () {
    munmap (mm, fs);
    close (fd);
}

int main () {
    const char* filename = "tmp.byte";
    read_file (filename);
    cek ();
    clean_up ();
    return 0;
}
