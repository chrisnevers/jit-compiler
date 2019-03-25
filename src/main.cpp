#include <iostream>
#include <string>
#include <stdexcept>
#include "ast.h"

using namespace std;

const int heap_size = 1 << 14;
size_t heap[heap_size] = {};
int heap_max = heap_size - 1;

int pc = 0;
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

K* k_ret () {
    KRet* node  = (KRet*) malloc (sizeof (KRet));
    node->k     = mk_k (TKRet);
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

    size_t pc = heap[pf - 1];

    while (true) {
        M* m = (struct M*) pc;
        int tag = m->tag;

        if (is_v (tag) && pe->tag == TEMt) {
            cout << "Result: "; display_m (tag, (M*) pc);
            return;
        }

        switch (tag) {
            case TMNul:
            case TMClo:
            case TMNum: {
                pe = e_mt ();
                switch (pk->tag) {
                    case TKRet: {
                        cout << "KRet" << endl;
                        break;
                    }
                    default: {
                        cout << "Unknown K tag: " << tag << endl;
                        break;
                    }
                }
                break;
            }
            case TMLam: {
                MClo* clo   = (MClo*) malloc (sizeof (MClo));
                clo->m      = mk_m (TMClo);
                clo->ex     = (M*) pc;
                clo->env    = pe;
                pc          = (size_t) clo;
                pe          = e_mt ();
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
    pc = stoi (tmp, nullptr, 2);

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
                break;
            }
            case TMVar: {
                fscanf (fp, "%s", tmp);
                MVar* node  = (MVar*) malloc (sizeof (MVar));
                node->m     = mk_m (TMVar);
                node->id    = stoi (tmp, nullptr, 2);
                heap[i++]   = (size_t) node;
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
                break;
            }
            case TMApp: {
                fscanf (fp, "%s", tmp);
                MApp* node  = (MApp*) malloc (sizeof (MApp));
                node->m     = mk_m (TMApp);
                // node->
            }
            default: {
                cout << "Unknown tag: " << tag << endl;
                break;
            }
        }
    }

    pf = i;

    return;
}

int main () {
    read_file ("tmp.byte");
    cek ();
    return 0;
}
