#include "ast.h"
#include "gc.h"

bool is_v (size_t tag) {
    switch (tag) {
        case TMTru:
        case TMFals:
        case TMNum:
        case TMPair:
        case TMClo: return true;
        default: return false;
    }
}

M* new_m (M** m) {
    switch ((*m)->tag) {
        case TMNul: {
            return m_nul();
        }
        case TMNum: {
            MNum* num = (MNum*) malloc1 (sizeof(MNum));
            MNum* old = (MNum*) *m;
            num->m = mk_m(TMNum);
            num->val = old->val;
            return (M*) num;
        }
        case TMVar: {
            MVar* var = (MVar*) malloc1 (sizeof(MVar));
            MVar* old = (MVar*) *m;
            var->m = mk_m(TMVar);
            var->id = old->id;
            return (M*) var;
        }
        case TMTru: {
            MTru* tru = (MTru*) malloc1 (sizeof(MTru));
            tru->m = mk_m (TMTru);
            return (M*) tru;
        }
        case TMFals: {
            MFals* fals = (MFals*) malloc1 (sizeof(MFals));
            fals->m = mk_m (TMFals);
            return (M*) fals;
        }
        case TMPair: {
            MPair* pair = (MPair*) malloc1 (sizeof(MPair));
            MPair* old = (MPair*) *m;
            pair->m = mk_m (TMPair);
            pair->l = new_m(&old->l);
            pair->r = new_m(&old->r);
            return (M*) pair;
        }
        case TMClo: {
            MClo* clo = (MClo*) malloc1 (sizeof(MClo));
            MClo* old = (MClo*) *m;
            clo->m = mk_m (TMClo);
            clo->env = old->env;
            clo->ex = new_m (&old->ex);
            return (M*) clo;
        }
        default: {
            std::cout << "New_m: no match" << (*m)->tag << std::endl;
            abort();
        }
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
