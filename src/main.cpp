#include <iostream>
#include <stdexcept>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "ast.h"
#include "gc.h"

using namespace std;

bool debug = true;

// Temp string for reading memory
char* tmp = (char*) malloc (8 * sizeof (char));
char* mm;       // Mapped memory location
int fd;         // Bytecode program file descriptor
int fs;         // Bytecode program file size

// Initialize garbage collector heap
size_t heap[heap_size] = {};
size_t heap_max = heap_size - 1;

size_t* from_space_begin = heap;
size_t* from_space_end = heap + (heap_size / 2) - 1;
size_t* to_space_begin = heap + (heap_size / 2);
size_t* to_space_end = heap + heap_size;
size_t* free_ptr = from_space_begin;

size_t pf = 0;
M* pc = 0;
E* pe;
K* pk;

void cek () {
    pe = e_mt();
    pk = k_ret();

    while (true) {
        M* m = pc;
        int tag = m->tag;

        display_state (pc, pe, pk);

        switch (tag) {
            case TMLam: {
                MClo* clo   = (MClo*) malloc1 (3);
                clo->m      = mk_m (TMClo);
                clo->ex     = pc;
                clo->env    = pe;
                pc          = (M*) clo;
                pe          = e_mt ();
                break;
            }
            case TMApp: {
                MApp* app = (MApp*) m;
                pc = app->fn;
                pk = k_fn (&app->arg, &pe, &pk);
                break;
            }
            case TMVar: {
                MVar* var = (MVar*) m;
                E* e = pe;
                M* old_pc = pc;
                while (e->tag != TEMt) {
                    EClo* clo = (EClo*) e;
                    if (var->id == clo->id) {
                        pc = clo->val;
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
                        pk = k_arg (pc, &fn->ok);
                        pc = fn->m;
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
                            pc = lam->body;
                            pe = ne;
                            pk = ar->ok;
                        } else {
                            throw logic_error ("Expected Closure in KArg");
                        }
                        break;
                    }
                    case TKOp1: {
                        MNum* val = (MNum*) malloc1 (2);
                        KOp1* op = (KOp1*) pk;
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
                        pc = (M*) val;
                        pe = e_mt ();
                        pk = op->ok;
                        break;
                    }
                    case TKOp2: {
                        KOp2** op = (KOp2**) &pk;
                        // Solve second value now
                        if ((*op)->m->tag != TMNul) {
                            KOp2* nk = (KOp2*) malloc1 (5);
                            nk->k   = (*op)->k;
                            nk->op  = (*op)->op;
                            nk->v   = pc;
                            nk->m   = m_nul ();
                            nk->ok  = (*op)->ok;
                            pc = (*op)->m;
                            pk = (K*) nk;
                        } else {
                            // Values have been computed
                            MNum* val = (MNum*) malloc1 (2);
                            val->m      = mk_m (TMNum);
                            switch ((*op)->op) {
                                case TPAdd: {
                                    MNum* l = (MNum*) (*op)->v;
                                    MNum* r = (MNum*) pc;
                                    val->val = l->val + r->val;
                                    break;
                                }
                                case TPSub: {
                                    MNum* l = (MNum*) (*op)->v;
                                    MNum* r = (MNum*) pc;
                                    val->val = l->val - r->val;
                                    break;
                                }
                                default: {
                                    throw logic_error ("Unknown operator");
                                }
                            };
                            pc = (M*) val;
                            pe = e_mt ();
                            pk = (*op)->ok;
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
                MPrm** prm = (MPrm**) &pc;
                switch ((*prm)->arity) {
                    case 0: {
                        switch ((*prm)->op) {
                            case TPRead: {
                                int i = 0;
                                scanf ("%d", &i);
                                MNum* n = (MNum*) malloc1 (2);
                                n->m    = mk_m (TMNum);
                                n->val  = i;
                                pc = (M*) n;
                                break;
                            }
                            default: {
                                throw logic_error ("Unknown primitive op");
                            }
                        }
                        break;
                    }
                    case 1: {
                        KOp1* kop = (KOp1*) malloc1 (4);
                        kop->k  = mk_k (TKOp1);
                        kop->op = (*prm)->op;
                        kop->v  = m_nul();
                        kop->ok = pk;
                        pc = (*prm)->ms[0];
                        pk = (K*) kop;
                        break;
                    }
                    case 2: {
                        KOp2* kop = (KOp2*) malloc1 (5);
                        kop->k  = mk_k (TKOp2);
                        kop->op = (*prm)->op;
                        kop->m  = (*prm)->ms[1];
                        kop->ok = pk;
                        kop->v  = m_nul ();
                        pc  = (*prm)->ms[0];
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
            MNum* n = (MNum*) malloc1 (2);
            n->m    = mk_m (TMNum);
            n->val  = get_int (tmp, pos, 1);
            return (M*) n;
        }
        case TMVar: {
            MVar* n = (MVar*) malloc1 (2);
            n->m    = mk_m (TMVar);
            n->id   = get_int (tmp, pos, 1);
            return (M*) n;
        }
        case TMApp: {
            MApp* n = (MApp*) malloc1 (3);
            n->m    = mk_m (TMApp);
            n->fn   = load_obj (get_int (tmp, pos, 1));
            n->arg  = load_obj (get_int (tmp, pos, 2));
            return (M*) n;
        }
        case TMLam: {
            MLam* n = (MLam*) malloc1 (3);
            n->m    = mk_m (TMLam);
            n->id   = get_int (tmp, pos, 1);
            n->body = load_obj (get_int (tmp, pos, 2));
            return (M*) n;
        }
        case TMPrm: {
            MPrm* n = (MPrm*) malloc1 (4);
            n->m    = mk_m (TMPrm);
            n->op   = get_int (tmp, pos, 1);
            switch (n->op) {
                case TPSub:
                case TPAdd: {
                    n->arity = 2;
                    n->ms    = (M**) malloc1 (2);
                    n->ms[0] = load_obj (get_int (tmp, pos, 2));
                    n->ms[1] = load_obj (get_int (tmp, pos, 3));
                    break;
                }
                case TPNeg: {
                    n->arity = 1;
                    n->ms    = (M**) malloc1 (1);
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
    pf      = get_int (tmp, -2, 0);
    int pos = get_int (tmp, -1, 0);

    // Load structures for program
    pc = load_obj (pos);

    // print_space (from_space_begin, from_space_end, "FROM SPACE");

    return;
}

void clean_up () {
    munmap (mm, fs);
    close (fd);
}

int main () {
    read_file ("tmp.byte");
    cek ();
    clean_up ();
    return 0;
}
