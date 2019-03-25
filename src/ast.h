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
    TKRet = 200
};

struct E {
    int tag;
};

struct EMt {
    E e;
};

void display_e (int tag, E* e);

struct M {
    int tag;
};

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

void display_k (int tag, K* k);

#endif
