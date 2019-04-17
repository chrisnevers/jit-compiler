#ifndef AST_HEADER
#define AST_HEADER

#include <iostream>

enum {
    TMNul = 0,
    TMNum = 1,
    TMVar = 2,
    TMApp = 3,
    TMLam = 4,
    TMClo = 5,
    TMPrm = 6,
    TMTru = 7,
    TMFals = 8,
    TMPair = 9,
    TMIf   = 10,
    TPSub  = 50,
    TPAdd  = 51,
    TPNeg  = 52,
    TPRead = 53,
    TPMkPair = 54,
    TPFst = 55,
    TPSnd = 56,
    TPGt  = 57,
    TPGtE = 58,
    TPLt  = 59,
    TPLtE = 60,
    TPEq  = 61,
    TEMt  = 100,
    TEClo = 101,
    TKRet = 200,
    TKFn  = 201,
    TKArg = 202,
    TKOp0 = 203,
    TKOp1 = 204,
    TKOp2 = 205,
    TKIf  = 206,
};

struct E {
    size_t tag;
};

struct M {
    size_t tag;
};

struct EMt {
    E e;
};

struct EClo {
    E e;
    M* val;
    E* nxt;
    int id;
};

void display_e (size_t tag, E* e);

struct MNul {
    M m;
};

struct MTru {
    M m;
};

struct MFals {
    M m;
};

struct MPair {
    M m;
    M* l;
    M* r;
};

struct MNum {
    M m;
    int val;
};

struct MVar {
    M m;
    int id;
};

struct MClo {
    M m;
    M* ex;
    E* env;
};

struct MLam {
    M m;
    M* body;
    int id;
};

struct MApp {
    M m;
    M* fn;
    M* arg;
};

struct MPrm {
    M m;
    M** ms;
    int arity;
    char op;
};

struct MIf {
    M m;
    M* cnd;
    M* thn;
    M* els;
};

void display_m (size_t tag, M* m, bool nl=true);

struct K {
    size_t tag;
};

struct KRet {
    K k;
};

struct KFn {
    K k;
    M* m;
    E* e;
    K* ok;
};

struct KArg {
    K k;
    M* m;
    K* ok;
};

struct KOp0 {
    K k;
    K* ok;
    char op;
};

struct KOp1 {
    K k;
    M* v;
    K* ok;
    char op;
};

struct KOp2 {
    K k;
    M* v;
    M* m;
    K* ok;
    char op;
};

struct KIf {
    K k;
    M* t;
    M* e;
    K* ok;
};

void display_k (size_t tag, K* k);

void display_state (M* m, E* pe, K* pk);
void display_heap ();

bool is_v (size_t tag);

M* new_m (M** m);
M mk_m (size_t tag);
E mk_e (size_t tag);
K mk_k (size_t tag);
M* m_nul ();
E* e_mt ();
K* k_ret ();
K* k_fn (M** m, E** e, K** ok);

#endif
