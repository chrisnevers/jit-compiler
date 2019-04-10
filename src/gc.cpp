#include <iostream>
#include <stdexcept>
#include "gc.h"
#include "ast.h"


extern M* pc;
extern E* pe;
extern K* pk;

extern size_t heap[];
extern size_t* from_space_begin;
extern size_t* from_space_end;
extern size_t* to_space_begin;
extern size_t* to_space_end;
extern size_t* free_ptr;

size_t* queue_head;
size_t* queue_tail;

using namespace std;

void print_space (size_t* begin, size_t* end, const char* label) {
    cout << "\n" << label << endl;
    size_t* ptr = begin;
    while (ptr != end) {
        printf ("%lu: %lu\n", (size_t)ptr, *ptr);
        ptr++;
    }
}

bool is_forwarding_ptr (size_t* ptr) {
    return (ptr >= to_space_begin && ptr < to_space_end);
}

bool is_from_space_ptr (size_t* ptr) {
    return (ptr >= from_space_begin && ptr < from_space_end);
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
    size_t* to_ptr = queue_tail;

    // Copy the tag
    to_ptr[0] = tag;

    // Depending on what the node is, copy of it's contents
    // Then, update the position of queue_tail to the next free space
    switch (tag) {
        case TMNul: {
            queue_tail += 1;
            break;
        }
        case TMNum: {
            MNum* num = (MNum*) m;
            to_ptr[1] = (size_t)num->val;
            queue_tail += 2;
            break;
        }
        case TMVar: {
            MVar* var = (MVar*) m;
            to_ptr[1] = (size_t)var->id;
            queue_tail += 2;
            break;
        }
        case TMApp: {
            MApp* app = (MApp*) m;
            to_ptr[1] = (size_t)app->fn;
            to_ptr[2] = (size_t)app->arg;
            queue_tail += 3;
            break;
        }
        case TMLam: {
            MLam* lam = (MLam*) m;
            to_ptr[1] = lam->id;
            to_ptr[2] = (size_t)lam->body;
            queue_tail += 3;
            break;
        }
        case TMClo: {
            MClo* clo = (MClo*) m;
            to_ptr[1] = (size_t)clo->ex;
            to_ptr[2] = (size_t)clo->env;
            queue_tail += 3;
            break;
        }
        case TMPrm: {
            MPrm* prm = (MPrm*) m;
            to_ptr[1] = prm->op;
            to_ptr[2] = prm->arity;
            // prm->ms points to an array of node pointers.
            // We are going to copy them right now, starting
            // from the next address.
            to_ptr[3] = (size_t)&to_ptr[4];
            for (int i = 0; i < prm->arity; ++i) {
                to_ptr[i + 4] = (size_t)prm->ms[i];
            }
            queue_tail += prm->arity + 4;
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
    size_t* to_ptr = queue_tail;

    // Copy the tag
    to_ptr[0] = tag;

    switch (tag) {
        case TEMt: {
            queue_tail += 1;
            break;
        }
        case TEClo: {
            EClo* clo = (EClo*) e;
            to_ptr[1] = clo->id;
            to_ptr[2] = (size_t) clo->val;
            to_ptr[3] = (size_t) clo->nxt;
            queue_tail += 4;
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
    size_t* to_ptr = queue_tail;

    // Copy the tag
    to_ptr[0] = tag;

    switch (tag) {
        case TKRet: {
            queue_tail += 1;
            break;
        }
        case TKFn: {
            KFn* fn = (KFn*) k;
            to_ptr[1] = (size_t)fn->m;
            to_ptr[2] = (size_t)fn->e;
            to_ptr[3] = (size_t)fn->ok;
            queue_tail += 4;
            break;
        }
        case TKArg: {
            KArg* arg = (KArg*) k;
            to_ptr[1] = (size_t) arg->m;
            to_ptr[2] = (size_t) arg->ok;
            queue_tail += 3;
            break;
        }
        case TKOp0: {
            KOp0* op = (KOp0*) k;
            to_ptr[1] = op->op;
            to_ptr[2] = (size_t) op->ok;
            queue_tail += 3;
            break;
        }
        case TKOp1: {
            KOp1* op = (KOp1*) k;
            to_ptr[1] = op->op;
            to_ptr[2] = (size_t) op->v;
            to_ptr[3] = (size_t) op->ok;
            queue_tail += 4;
            break;
        }
        case TKOp2: {
            KOp2* op = (KOp2*) k;
            to_ptr[1] = op->op;
            to_ptr[2] = (size_t) op->v;
            to_ptr[3] = (size_t) op->m;
            to_ptr[4] = (size_t) op->ok;
            queue_tail += 5;
            break;
        }
    }
    // Update the tag of the old object to point to the new object in to_space
    k->tag = (size_t)to_ptr;
    // Update the original pointer to point to the newly copied object.
    *k_ptr = (K*) to_ptr;
}

void swap_spaces () {
    size_t* tmp_begin = to_space_begin;
    size_t* tmp_end   = to_space_end;
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
    size_t* qh = queue_head;

    switch (tag) {
        // Nothing to process for these nodes
        case TKRet:
        case TEMt :
        case TMNul: {
            queue_head++;
            break;
        }
        // Skip processing these nodes because they do not contain pointers
        case TMNum:
        case TMVar: {
            queue_head += 2;
            break;
        }
        case TMApp: {
            MApp* app = (MApp*) *q_ptr;
            process_m (&app->fn);
            process_m (&app->arg);
            queue_head += 3;
            break;
        }
        case TMLam: {
            MLam* lam = (MLam*) *q_ptr;
            process_m (&lam->body);
            queue_head += 3;
            break;
        }
        case TMClo: {
            MClo* clo = (MClo*) *q_ptr;
            process_m (&clo->ex);
            process_e (&clo->env);
            queue_head += 3;
            break;
        }
        case TMPrm: {
            MPrm* prm = (MPrm*) *q_ptr;
            for (int i = 0; i < prm->arity; i++) {
                process_m (&prm->ms[i]);
            }
            queue_head += 4 + prm->arity;
            break;
        }
        case TEClo: {
            EClo* clo = (EClo*) *q_ptr;
            process_m (&clo->val);
            process_e (&clo->nxt);
            queue_head += 4;
            break;
        }
        case TKFn : {
            KFn* fn = (KFn*) *q_ptr;
            process_m (&fn->m);
            process_e (&fn->e);
            process_k (&fn->ok);
            queue_head += 4;
            break;
        }
        case TKArg: {
            KArg* arg = (KArg*) *q_ptr;
            process_m (&arg->m);
            process_k (&arg->ok);
            queue_head += 3;
            break;
        }
        case TKOp0: {
            KOp0* op = (KOp0*) *q_ptr;
            process_k (&op->ok);
            queue_head += 3;
            break;
        }
        case TKOp1: {
            KOp1* op = (KOp1*) *q_ptr;
            process_m (&op->v);
            process_k (&op->ok);
            queue_head += 4;
            break;
        }
        case TKOp2: {
            KOp2* op = (KOp2*) *q_ptr;
            process_m (&op->v);
            process_m (&op->m);
            process_k (&op->ok);
            queue_head += 5;
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

    // Enter loop to process the tuple at front of queue
    while (queue_head != queue_tail) {
        process(&queue_head);
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
                << size * sizeof (size_t) << " bytes" << endl;
        }
    }
    size_t* ret = free_ptr;
    free_ptr += size;
    return ret;
}
