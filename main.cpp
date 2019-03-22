#include <iostream>
#include <stdexcept>

using namespace std;

// Set up heap for storing terms
const int heap_size = 1 << 14;
int heap[heap_size] = {};
int heap_max = heap_size - 1;

int pc = 0;
int pe = 0;
int pk = 0;
int pf = 0;

enum {
    M_Nul = 0
  , M_Con = 1
  , M_Var = 2
  , M_App = 3
  , M_Abs = 4
  , M_Prm = 5
  , M_Clo = 6
  , E_Mt  = 100
  , E_Clo = 101
  , K_Ret = 200
  , K_Fn  = 201
  , K_Ar  = 202
  , K_Pr  = 203
  , K_PrM = 204
  , K_PrV = 205
  , K_Op  = 206
  , P_Add = 300
};

void display_obj (int p) {
  switch (heap [p]) {
    case M_Nul: cout << "Null"; break;
    case M_Con: cout << heap [p + 1]; break;
    case M_Var: cout << "v" << heap [p + 1]; break;
    case M_Abs:
      cout << "λ (v" << heap [p + 1];
      cout << ") {";
      display_obj (heap [p + 2]);
      cout << "}";
      break;
    case M_App:
      cout << "(";
      display_obj (heap [p + 1]);
      cout << " ";
      display_obj (heap [p + 2]);
      cout << ")";
      break;
    case M_Clo: cout << "Clo (" << heap [p + 1] << ", " << heap [p + 2] << ")";
      break;
    case M_Prm:
      cout << "Prm (";
      display_obj (p + 1);
      cout << ", ";
      display_obj (heap [p + 2]);
      cout << ", ";
      display_obj (heap [p + 3]);
      cout << ")";
      break;
    case P_Add: cout << "+"; break;
    case E_Mt: cout << "*"; break;
    case E_Clo:
      display_obj (heap [p + 3]);
      cout << "[";
      display_obj (heap [p + 1]);
      cout << " → ";
      display_obj (heap [p + 2]);
      cout << "]";
      break;
    case K_Op:
      cout << "Op (";
      display_obj (heap[p + 1]);
      cout << ", ";
      display_obj (heap[p + 2]);
      cout << ", ";
      display_obj (heap[p + 3]);
      cout << ")";
      break;
    case K_Ret: cout << "Ret"; break;
    case K_Ar:
      cout << "Ar (";
      display_obj (heap[p + 1]);
      cout << ", ";
      display_obj (heap[p + 2]);
      cout << ")";
      break;
    case K_Fn:
      cout << "Fn (";
      display_obj (heap[p + 1]);
      cout << ", ";
      display_obj (heap[p + 2]);
      cout << ", ";
      display_obj (heap[p + 3]);
      cout << ")";
      break;
    case K_Pr: cout << "Pr ()"; break;
    case K_PrM:
      cout << "PrM (";
      display_obj (heap[p + 1]);
      cout << ", ";
      display_obj (heap[p + 2]);
      cout << ", ";
      display_obj (heap[p + 3]);
      cout << ")";
      break;
    case K_PrV:
      cout << "PrV (";
      display_obj (p + 1);
      cout << ", ";
      display_obj (heap[p + 2]);
      cout << ")";
      break;
    default: cout << "Unknown: " << heap[p];
  }
}

void display_state () {
  cout << "[" << pf << "] <";
  display_obj (pc);
  cout << ", ";
  display_obj (pe);
  cout << ", ";
  display_obj (pk);
  cout << ">" << endl;
}

int alloc (int size) {
  if (pf + size > heap_max) {
    throw logic_error ("Out of memory");
  }
  pf += size;
  return pf - size;
}

void cek () {
  // Setup initial term
  pe = alloc (1);
  heap [pe] = E_Mt;
  pk = alloc (1);
  heap [pk] = K_Ret;

  while (true) {
    display_state ();
    getchar();
    switch (heap [pc]) {
      // <V, Σ, K> : process K
      case M_Nul:
      case M_Clo:
      case M_Con: {
        pe = 0;
        swap (pc, pk);
        break;
      }
      // <(oⁿ M N ...), Σ, Κ> → <M, Σ, Opd (["+"], [(N, Σ)], Κ)
      case M_Prm: {
        int op = heap [pc + 1];
        switch (op) {
          case P_Add: {
            int tbd = alloc (3);
            heap [tbd] = K_PrM;
            heap [tbd + 1] = heap [pc + 3];
            heap [tbd + 2] = pe;
            heap [tbd + 3] = M_Nul;
            cout << "TBD: "; display_obj (tbd); cout <<endl;
            int pd = alloc (2);
            heap [pd] = K_PrV;
            heap [pd + 1] = op;
            heap [pd + 2] = M_Nul;
            cout << "PD: "; display_obj (pd); cout <<endl;
            int nk = alloc (4);
            heap [nk] = K_Op;
            heap [nk + 1] = pd;
            heap [nk + 2] = tbd;
            heap [nk + 3] = pk;
            cout << "NK: "; display_obj (nk); cout <<endl;
            pc = heap [pc + 2];
            pk = nk;
            // swap (pc, pk);
            break;
          }
        }
        break;
      }

      // <X, Σ, K> → <c, K> where Σ[x] = c
      case M_Var: {
        // Get variable to search for
        int x = heap [pc + 1];
        // Store current eval in case of failure
        int ce = pe;
        while (heap [pe] == E_Clo) {
          // If variable is found, evaluate value, o.w. search next Σ
          if (heap [pe + 1] == x) {
            pc = heap [pe + 2];
            break;
          }
          pe = heap [pe + 3];
        }
        if (heap [pe] != E_Clo ) {
          pe = ce;
          display_state ();
          cout << "Unbound variable: " << x << endl;
          abort ();
        }
        pe = 0;
        break;
      }
      case M_Abs: {
        printf ("Abs\n");
        int clo = alloc (3);
        heap [clo + 0] = M_Clo;
        heap [clo + 1] = pc;
        heap [clo + 2] = pe;
        pc = clo;
        pe = 0;
        break;
      }
      case M_App: {
        printf ("Application\n");
        int nk = alloc (4);
        heap [nk + 0] = K_Fn;
        heap [nk + 1] = heap [pc + 2];
        heap [nk + 2] = pe;
        heap [nk + 3] = pk;
        pk = nk;
        pc = heap [pc + 1];
        break;
      }
      // <V, Σ, Ar (Clo(λX.M, Σ'), K)> → <M, Σ'[x→v], K>
      case K_Ar: {
        swap (pc, pk);
        int target = heap [pk + 1];
        int lam = heap [target + 1];
        int ne = alloc (4);
        heap [ne + 0] = E_Clo;
        // Variable Name
        heap [ne + 1] = heap [lam + 1];
        // Value
        heap [ne + 2] = pc;
        // rest of env Σ'
        heap [ne + 3] = heap [target + 2];
        // Evaluate M of closure
        pc = heap [lam + 2];
        pe = ne;
        pk = heap [pk + 2];
        break;
      }
      // <V, Σ, Fn (N, Σ', K) → <N, Σ', Ar (V, K)>
      case K_Fn: {
        swap (pc, pk);
        int nk = alloc (3);
        heap [nk] = K_Ar;
        heap [nk + 1] = pc;
        heap [nk + 2] = heap [pk + 3];
        pc = heap [pk + 1];
        pe = heap [pk + 2];
        pk = nk;
        break;
      }
      case K_Op: {
        int tbd = heap [pc + 2];
        display_obj (tbd); cout << endl;
        if (heap[tbd + 1] == M_Nul) {
          // All b
          cout << "ready to add" << endl;

        } else {
          cout << "PC: ";
          display_obj (pc);
          cout << endl;
          cout << "PE: ";
          display_obj (pe);
          cout << endl;
          cout << "PK: ";
          display_obj (pk);
          cout << endl;
          // Solve V
          // <V, Σ, Opd ([v..o], [(N, Σ') ...], K)>
          //  → <N, Σ', Opd (V@[v...o], [c...], K)
          cout << "Solve v" << endl;
          cout << "(N Σ'): ";
          display_obj (tbd);
          cout << endl;
          pc = heap [tbd + 1]; // guuch
          pe = heap [tbd + 2]; // guuch

          // Append value to computed list
          int pd = alloc (2);
          heap [pd] = K_PrV;
          heap [pd + 1] = heap [pk + 1];
          heap [pd + 2] = heap [pk + 1];
          cout << "(V @ [v]): ";
          display_obj (pd);
          cout << endl;

          // cout << "PC: ";
          // display_obj (heap[tbd + 1]);
          // cout << "\nPE: ";
          // display_obj (heap[tbd + 2]);
          // cout << "\nPK: ";
          // display_obj (heap[pc + 3]);
          // cout << endl;
          // int pd = alloc (2);
          // heap [pd] = K_PrV;
          // heap [pd + 1] = heap [pk];
          // heap [pd + 2] = heap [pc + 2];
          pk = heap [pc + 3];
          // display_obj (pd);
          // cout << endl;
          // heap [pk + 2]
        }

        swap (pc, pk);

        break;
      }
      case K_Ret: {
        swap (pc, pk);
        display_state();
        cout << pf << " - Result: " << heap[pc+1] << endl;
        return;
      }
    }
  }
}

/**
 * Reads in bytecode file
 */
void read_file (const char* filename) {
  FILE* fp;
  fp = fopen (filename, "r");
  fscanf (fp, "%d", &pf);
  fscanf (fp, "%d", &pc);

  for (int i = 0; i < pf; ++i) {
    fscanf (fp, "%d", &heap[i]);
  }

  fclose (fp);

  return;
}

int main() {
  read_file ("tmp.byte");
  cek ();
  return 0;
}
