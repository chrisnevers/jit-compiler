#include "ast.h"
#include "gc.h"

bool is_v (size_t tag) {
    switch (tag) {
        case TMNum: return true;
        case TMClo: return true;
        default: return false;
    }
}

M mk_m (size_t tag) {
    return (M) { .tag = tag };
}

E mk_e (size_t tag) {
    return (E) { .tag = tag };
}

K mk_k (size_t tag) {
    return (K) { .tag = tag };
}

M* m_nul () {
    MNul* node  = (MNul*) malloc1 (sizeof(MNul));
    node->m     = mk_m (TMNul);
    return (M*) node;
}

E* e_mt () {
    EMt* node   = (EMt*) malloc1 (sizeof(EMt));
    node->e     = mk_e (TEMt);
    return (E*) node;
}

K* k_ret () {
    KRet* node  = (KRet*) malloc1 (sizeof(KRet));
    node->k     = mk_k (TKRet);
    return (K*) node;
}
