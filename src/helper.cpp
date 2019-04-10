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
    MNul* node  = (MNul*) malloc1 (1);
    node->m     = mk_m (TMNul);
    return (M*) node;
}

E* e_mt () {
    EMt* node   = (EMt*) malloc1 (1);
    node->e     = mk_e (TEMt);
    return (E*) node;
}

E* e_clo (size_t id, M* val, E* next) {
    EClo* node  = (EClo*) malloc1 (4);
    node->e     = mk_e (TEClo);
    node->id    = id;
    node->val   = val;
    node->nxt   = next;
    return (E*) node;
}

K* k_ret () {
    KRet* node  = (KRet*) malloc1 (1);
    node->k     = mk_k (TKRet);
    return (K*) node;
}

K* k_fn (M** m, E** e, K** ok) {
    KFn* node   = (KFn*) malloc1 (4);
    node->k     = mk_k (TKFn);
    node->m     = *m;
    node->e     = *e;
    node->ok    = *ok;
    return (K*) node;
}

K* k_arg (M* m, K** ok) {
    KArg* node   = (KArg*) malloc1 (3);
    node->k     = mk_k (TKArg);
    node->m     = m;
    node->ok    = *ok;
    return (K*) node;
}
