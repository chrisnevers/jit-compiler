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
    TPSub = 50,
    TPAdd = 51,
    TPNeg = 52,
    TPRead = 53,
    TEMt  = 100,
    TEClo = 101,
    TKRet = 200,
    TKFn  = 201,
    TKArg = 202,
    TKOp0 = 203,
    TKOp1 = 204,
    TKOp2 = 205,
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
    size_t id;
    M* val;
    E* nxt;
};

void display_e (size_t tag, E* e);

struct MNul {
    M m;
};

struct MNum {
    M m;
    size_t val;
};

struct MVar {
    M m;
    size_t id;
};

struct MClo {
    M m;
    M* ex;
    E* env;
};

struct MLam {
    M m;
    size_t id;
    M* body;
};

struct MApp {
    M m;
    M* fn;
    M* arg;
};

struct MPrm {
    M m;
    size_t op;
    size_t arity;
    M** ms;
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
    size_t op;
    K* ok;
};

struct KOp1 {
    K k;
    size_t op;
    M* v;
    K* ok;
};

struct KOp2 {
    K k;
    size_t op;
    M* v;
    M* m;
    K* ok;
};

void display_k (size_t tag, K* k);

void display_state (M* m, E* pe, K* pk);
void display_heap ();

bool is_v (size_t tag);

M mk_m (size_t tag);
E mk_e (size_t tag);
K mk_k (size_t tag);
M* m_nul ();
E* e_mt ();
E* e_clo (size_t id, M* val, E* next);
K* k_ret ();
K* k_fn (M** m, E** e, K** ok);
K* k_arg (M* m, K** ok);

#endif
