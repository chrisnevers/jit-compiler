#include "ast.h"
#include "gc.h"

bool is_v (int tag) {
    switch (tag) {
        case TMNum: return true;
        case TMClo: return true;
        default: return false;
    }
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
    MNul* node  = (MNul*) malloc1 (sizeof (MNul));
    node->m     = mk_m (TMNul);
    return (M*) node;
}

E* e_mt () {
    EMt* node   = (EMt*) malloc1 (sizeof (EMt));
    node->e     = mk_e (TEMt);
    return (E*) node;
}

E* e_clo (int id, M* val, E* next) {
    EClo* node  = (EClo*) malloc1 (sizeof (EClo));
    node->e     = mk_e (TEClo);
    node->id    = id;
    node->val   = val;
    node->nxt   = next;
    return (E*) node;
}

K* k_ret () {
    KRet* node  = (KRet*) malloc1 (sizeof (KRet));
    node->k     = mk_k (TKRet);
    return (K*) node;
}

K* k_fn (M* m, E* e, K* ok) {
    KFn* node   = (KFn*) malloc1 (sizeof (KFn));
    node->k     = mk_k (TKFn);
    node->m     = m;
    node->e     = e;
    node->ok    = ok;
    return (K*) node;
}

K* k_arg (M* m, K* ok) {
    KArg* node   = (KArg*) malloc1 (sizeof (KArg));
    node->k     = mk_k (TKArg);
    node->m     = m;
    node->ok    = ok;
    return (K*) node;
}
