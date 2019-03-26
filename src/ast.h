#ifndef AST_HEADER
#define AST_HEADER


enum {
    TMNul = 0,
    TMNum = 1,
    TMVar = 2,
    TMApp = 3,
    TMLam = 4,
    TMClo = 5,
    TEMt  = 100,
    TEClo = 101,
    TKRet = 200,
    TKFn  = 201,
    TKArg = 202
};

struct E {
    int tag;
};

struct M {
    int tag;
};

struct EMt {
    E e;
};

struct EClo {
    E e;
    int id;
    M* val;
    E* nxt;
};

void display_e (int tag, E* e);

struct MNul {
    M m;
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
    int id;
    M* body;
};

struct MApp {
    M m;
    M* fn;
    M* arg;
};

void display_m (int tag, M* m, bool nl=true);

struct K {
    int tag;
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

void display_k (int tag, K* k);

#endif
