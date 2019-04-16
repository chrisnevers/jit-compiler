#include <iostream>
#include <stdexcept>
#include "gc.h"
#include "ast.h"


extern M* pc;
extern E* pe;
extern K* pk;

extern char heap[];
extern char* from_space_begin;
extern char* from_space_end;
extern char* to_space_begin;
extern char* to_space_end;
extern char* free_ptr;

char* queue_head;
char* queue_tail;

using namespace std;

void print_space (char* begin, char* end, const char* label) {
    cout << "\n" << label << endl;
    char* ptr = begin;
    while (ptr != end) {
        size_t* tag = (size_t*)ptr;
        printf ("%lu: %lu - %c\n", (size_t)ptr, tag[0], *ptr);
        ptr++;
    }
}

bool is_forwarding_ptr (size_t* ptr) {
    return (ptr >= (size_t*)to_space_begin && ptr < (size_t*)to_space_end);
}

bool is_from_space_ptr (size_t* ptr) {
    return (ptr >= (size_t*)from_space_begin && ptr < (size_t*)from_space_end);
}

void copy_m (M** m_ptr) {
    // Access object and its tag
    M* m = *m_ptr;
    size_t tag = m->tag;

    // If the node has already been copied, then the
    // tag wil be a pointer to the new address, in that
    // case we do not need to copy anything. Just
    // change old pointer to new address
    if (is_forwarding_ptr ((size_t*)tag)) {
        *m_ptr = (M*)tag;
        return;
    }

    // We need to copy the node to to_space.
    // queue_tail is where there's free space.
    size_t* to_ptr = (size_t*)queue_tail;

    // Copy the tag
    to_ptr[0] = tag;

    // Depending on what the node is, copy of it's contents
    // Then, update the position of queue_tail to the next free space
    switch (tag) {
        case TMNul: { queue_tail += sizeof(MNul); break; }
        case TMTru: { queue_tail += sizeof(MTru); break; }
        case TMFals: { queue_tail += sizeof(MFals); break; }
        case TMNum: {
            MNum* num = (MNum*) m;
            int* i_ptr = (int*) (to_ptr + 1);
            i_ptr[0] = num->val;
            queue_tail += sizeof(MNum);
            break;
        }
        case TMVar: {
            MVar* var = (MVar*) m;
            int* i_ptr = (int*) (to_ptr + 1);
            i_ptr[0] = var->id;
            queue_tail += sizeof(MVar);
            break;
        }
        case TMPair: {
            MPair* pair = (MPair*) m;
            to_ptr[1] = (size_t)pair->l;
            to_ptr[2] = (size_t)pair->r;
            queue_tail += sizeof(MPair);
            break;
        }
        case TMApp: {
            MApp* app = (MApp*) m;
            to_ptr[1] = (size_t)app->fn;
            to_ptr[2] = (size_t)app->arg;
            queue_tail += sizeof(MApp);
            break;
        }
        case TMLam: {
            MLam* lam = (MLam*) m;
            to_ptr[1] = (size_t)lam->body;
            int* i_ptr = (int*) (to_ptr + 2);
            i_ptr[0] = lam->id;
            queue_tail += sizeof(MLam);
            break;
        }
        case TMClo: {
            MClo* clo = (MClo*) m;
            to_ptr[1] = (size_t)clo->ex;
            to_ptr[2] = (size_t)clo->env;
            queue_tail += sizeof(MClo);
            break;
        }
        case TMPrm: {
            MPrm* prm = (MPrm*) m;
            // prm->ms points to an array of node pointers.
            // We are going to copy them right now, starting
            // from the next address.
            to_ptr[1] = (size_t)&to_ptr[3];
            for (int i = 0; i < prm->arity; ++i) {
                to_ptr[i + 3] = (size_t)prm->ms[i];
            }
            int* i_ptr = (int*) (to_ptr + 2);
            i_ptr[0] = prm->arity;
            char* c_ptr = (char*) (i_ptr + 1);
            c_ptr[0] = prm->op;
            queue_tail += (prm->arity*sizeof(M*)) + sizeof(MPrm);
            break;
        }
    }

    // Update the tag of the old object to point to the new object in to_space
    m->tag = (size_t)to_ptr;
    // Update the original pointer to point to the newly copied object.
    *m_ptr = (M*) to_ptr;
}

void copy_e (E** e_ptr) {
    // Access object and its tag
    E* e = *e_ptr;
    size_t tag = e->tag;

    // If the node has already been copied, then the
    // tag wil be a pointer to the new address, in that
    // case we do not need to copy anything. Just
    // change old pointer to new address
    if (is_forwarding_ptr ((size_t*)tag)) {
        *e_ptr = (E*)tag;
        return;
    }

    // We need to copy the node to to_space.
    // queue_tail is where there's free space.
    size_t* to_ptr = (size_t*)queue_tail;

    // Copy the tag
    to_ptr[0] = tag;

    switch (tag) {
        case TEMt: {
            queue_tail += sizeof(EMt);
            break;
        }
        case TEClo: {
            EClo* clo = (EClo*) e;
            to_ptr[1] = (size_t) clo->val;
            to_ptr[2] = (size_t) clo->nxt;
            int* i_ptr = (int*) (to_ptr + 3);
            i_ptr[0] = clo->id;
            queue_tail += sizeof(EClo);
            break;
        }
    }
    // Update the tag of the old object to point to the new object in to_space
    e->tag = (size_t)to_ptr;
    // Update the original pointer to point to the newly copied object.
    *e_ptr = (E*) to_ptr;
}

void copy_k (K** k_ptr) {
    // Access object and its tag
    K* k = *k_ptr;  // Original address of pk
    size_t tag = k->tag;

    // If it has already been copied, change old pointer to new address
    if (is_forwarding_ptr ((size_t*)tag)) {
        *k_ptr = (K*)tag;
        return;
    }
    // We need to copy the node to to_space.
    // queue_tail is where there's free space.
    size_t* to_ptr = (size_t*)queue_tail;

    // Copy the tag
    to_ptr[0] = tag;

    switch (tag) {
        case TKRet: {
            queue_tail += sizeof(KRet);
            break;
        }
        case TKFn: {
            KFn* fn = (KFn*) k;
            to_ptr[1] = (size_t)fn->m;
            to_ptr[2] = (size_t)fn->e;
            to_ptr[3] = (size_t)fn->ok;
            queue_tail += sizeof(KFn);
            break;
        }
        case TKArg: {
            KArg* arg = (KArg*) k;
            to_ptr[1] = (size_t) arg->m;
            to_ptr[2] = (size_t) arg->ok;
            queue_tail += sizeof(KArg);
            break;
        }
        case TKOp0: {
            KOp0* op = (KOp0*) k;
            to_ptr[1] = (size_t) op->ok;
            char* c_ptr = (char*) (to_ptr + 2);
            c_ptr[0] = op->op;
            queue_tail += sizeof(KOp0);
            break;
        }
        case TKOp1: {
            KOp1* op = (KOp1*) k;
            to_ptr[1] = (size_t) op->v;
            to_ptr[2] = (size_t) op->ok;
            char* c_ptr = (char*) (to_ptr + 3);
            c_ptr[0] = op->op;
            queue_tail += sizeof(KOp1);
            break;
        }
        case TKOp2: {
            KOp2* op = (KOp2*) k;
            to_ptr[1] = (size_t) op->v;
            to_ptr[2] = (size_t) op->m;
            to_ptr[3] = (size_t) op->ok;
            char* c_ptr = (char*) (to_ptr + 4);
            c_ptr[0] = op->op;
            queue_tail += sizeof(KOp2);
            break;
        }
    }
    // Update the tag of the old object to point to the new object in to_space
    k->tag = (size_t)to_ptr;
    // Update the original pointer to point to the newly copied object.
    *k_ptr = (K*) to_ptr;
}

void swap_spaces () {
    char* tmp_begin = to_space_begin;
    char* tmp_end   = to_space_end;
    to_space_begin    = from_space_begin;
    to_space_end      = from_space_end;
    from_space_begin  = tmp_begin;
    from_space_end    = tmp_end;
}

void process_m (M** m) {
    if (is_from_space_ptr ((size_t*)*m)) {
        copy_m (m);
    }
}

void process_e (E** e) {
    if (is_from_space_ptr ((size_t*)*e)) {
        copy_e (e);
    }
}

void process_k (K** k) {
    if (is_from_space_ptr ((size_t*)*k)) {
        copy_k (k);
    }
}

void process (size_t** q_ptr) {
    // Node can either be M,E,K so just cast to size_t*
    size_t* node = (size_t*)*q_ptr;
    size_t tag = node[0];
    // Keep track of
    size_t* qh = (size_t*)queue_head;

    switch (tag) {
        // Nothing to process for these nodes
        case TKRet: { queue_head += sizeof(KRet); break; }
        case TEMt : { queue_head += sizeof(EMt); break; }
        case TMNul: { queue_head += sizeof(MNul); break; }
        case TMTru: { queue_head += sizeof(MTru); break; }
        case TMFals: { queue_head += sizeof(MFals); break; }
        // Skip processing these nodes because they do not contain pointers
        case TMNum: { queue_head += sizeof(MNum); break; }
        case TMVar: { queue_head += sizeof(MVar); break; }
        case TMPair: {
            MPair* pair = (MPair*) *q_ptr;
            process_m (&pair->l);
            process_m (&pair->r);
            queue_head += sizeof(MPair);
            break;
        }
        case TMApp: {
            MApp* app = (MApp*) *q_ptr;
            process_m (&app->fn);
            process_m (&app->arg);
            queue_head += sizeof(MApp);
            break;
        }
        case TMLam: {
            MLam* lam = (MLam*) *q_ptr;
            process_m (&lam->body);
            queue_head += sizeof(MLam);
            break;
        }
        case TMClo: {
            MClo* clo = (MClo*) *q_ptr;
            process_m (&clo->ex);
            process_e (&clo->env);
            queue_head += sizeof(MClo);
            break;
        }
        case TMPrm: {
            MPrm* prm = (MPrm*) *q_ptr;
            for (int i = 0; i < prm->arity; i++) {
                process_m (&prm->ms[i]);
            }
            queue_head += sizeof(MPrm) + (prm->arity*sizeof(M*));
            break;
        }
        case TEClo: {
            EClo* clo = (EClo*) *q_ptr;
            process_m (&clo->val);
            process_e (&clo->nxt);
            queue_head += sizeof(EClo);
            break;
        }
        case TKFn : {
            KFn* fn = (KFn*) *q_ptr;
            process_m (&fn->m);
            process_e (&fn->e);
            process_k (&fn->ok);
            queue_head += sizeof(KFn);
            break;
        }
        case TKArg: {
            KArg* arg = (KArg*) *q_ptr;
            process_m (&arg->m);
            process_k (&arg->ok);
            queue_head += sizeof(KArg);
            break;
        }
        case TKOp0: {
            KOp0* op = (KOp0*) *q_ptr;
            process_k (&op->ok);
            queue_head += sizeof(KOp0);
            break;
        }
        case TKOp1: {
            KOp1* op = (KOp1*) *q_ptr;
            process_m (&op->v);
            process_k (&op->ok);
            queue_head += sizeof(KOp1);
            break;
        }
        case TKOp2: {
            KOp2* op = (KOp2*) *q_ptr;
            process_m (&op->v);
            process_m (&op->m);
            process_k (&op->ok);
            queue_head += sizeof(KOp2);
            break;
        }
    }
    // *q_ptr = qh;
}

void collect () {
    // Setup queue in to space
    queue_head = queue_tail = to_space_begin;

    // Copy all nodes immediately reachable from root set (pc,pe,pk)
    // into to_space to form an initial queue
    copy_m (&pc);
    copy_e (&pe);
    copy_k (&pk);

    // print_space(to_space_begin, to_space_end, "to");
    // Enter loop to process the tuple at front of queue
    while (queue_head != queue_tail) {
        // cout << "process " << (size_t)queue_head << " : " << (size_t)queue_tail;
        process((size_t**)&queue_head);
    }

    // Set the free_ptr for future allocations
    free_ptr = queue_tail;

    // Swap the to and from space pointers
    swap_spaces ();

    // Clear the memory in to_space
    memset (to_space_begin, 0, half_size);
}

void* malloc1 (size_t size) {
    // printf ("Malloc called for %lu spaces\nFree ptr: %lu -> %lu\n", size,
    //     free_ptr, free_ptr + size);
    if (from_space_end - free_ptr < size) {
        cout << "Running Garbage Collection: " << size << endl;
        collect ();
        // Now that we performed garbage collection,
        // make sure we can perform the allocation
        if (from_space_end - free_ptr < size) {
            cout << "Cannot allocate the memory request for "
                << size << " bytes" << endl;
        }
    }
    char* ret = free_ptr;
    free_ptr += size;
    // printf ("Malloc returning: %lu\nNext free: %lu\n", (size_t)ret, (size_t)free_ptr);
    return ret;
}
